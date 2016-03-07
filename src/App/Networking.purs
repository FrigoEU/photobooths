module App.Networking where

import Prelude

import Database.AnyDB.SqlValue (toSql)
import Database.AnyDB
import Database.AnyDB.Transaction

import Data.Maybe
import Data.Date
import Data.Tuple
import Data.String 
import Data.Traversable
import Data.Array (filter, elemIndex)
import Data.Foreign
import Data.Foreign.Class
import Data.Either

import Node.Buffer (Buffer(), BUFFER())
import Node.FS.Aff (readdir, mkdir, writeFile, exists)
import Node.FS (FS())
import Node.Path (normalize, concat, basename)
import Node.OS

import Network.HTTP.Affjax 
import Network.HTTP.Method (Method(..))
 
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Class
import Control.Monad.Aff (runAff)
import Control.Monad.Error.Class (throwError, class MonadError)
import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Aff (Aff())
import Control.Alt
import Control.Bind (join)

import App.DB
import App.Endpoint
import App.Model.Photobooth
import App.Model.Event
import App.Model.SavedFile
import App.FS
import App.Config

main = do
  --let cname = "mycomputername"
  runAff (log <<< show) (const $ log "Everything synced!") $ withConnection networkingConnectionInfo $ \conn -> do
    fcf <- liftEff $ readConfigFile
    baseurl <- either (throwError <<< error <<< show) 
                      (\(WorkerConfig {webServiceHost}) -> return webServiceHost) fcf
    --let baseurl = "http://localhost:8080"
    cname <- liftEff $ hostname
    ------ For testing purposes ----- 
    dropDB conn
    makeDB conn
    
    liftEff $ log "Initial SQL Done"
    
    ------ Sync Photobooth ----------
    oldPb <- queryPhotobooth conn cname
    pbm <- execEndpoint_ baseurl getPhotobooth cname unit
    case pbm of
         Nothing -> throwError $ error $ "No photobooth instance found for: " <> cname
         Just pb -> execute_ (upsertPB pb) conn
    if oldPb /= pbm then rmdirRecur defaultDir else return unit
    liftEff $ log "Synced Photobooth(s)"
    
    ------ Sync Events --------------
    maybeLastUpdatedEvents <- queryOne getLastUpdateEvents [] conn
    lastUpdatedEvents <- defaultDate maybeLastUpdatedEvents (fromStringStrict "2010-01-01T00:00:00.000Z")
    pes <- execEndpoint_ baseurl getNewEvents (Tuple cname lastUpdatedEvents) unit
    traverse (\(PartialEvent {id: Just id}) -> rmdirRecur $ mkEventDir id) pes -- Event updated: regenerate folder
    withTransaction (\tc -> traverse (\pe -> execute_ (upsertPartialEvent pe) conn) pes) conn
    liftEff $ log "Synced Events"
    
    ------ Sync Files  --------------
    maybeLastUpdatedFiles <- queryOne getLastUpdateFiles [] conn
    lastUpdatedFiles <- defaultDate maybeLastUpdatedFiles (fromStringStrict "2010-01-01T00:00:00.000Z")
    files <- execEndpoint_ baseurl getNewFiles (Tuple cname lastUpdatedFiles) unit
    let eventIdsWithNewFiles = (\(SavedFile {eventId: i}) -> i) <$> files
    traverse (\i -> rmdirRecur $ mkEventDir i) eventIdsWithNewFiles -- File updated: regenerate event folder
    withTransaction (\tc -> traverse (\sf -> execute_ (upsertSavedFile sf) conn) files) conn
    liftEff $ log "Synced Files"
    
    ------ Sync File contents  --------------
    eventids <- query getEmptyFileEntries [] conn
    flip traverse eventids (\(WrappedId id) -> downloadFileById baseurl id >>= (\b -> saveFile conn id b))
    liftEff $ log "Synced File contents"
    
    ------ Sync statistics back to master  --------------
    stats <- queryAllStatistics conn cname
    execEndpoint_ baseurl postStatistics unit stats
    liftEff $ log "Synced statistics"
    
    ------ Make default profile folder -------------
    defaultFolderExists <- exists defaultDir
    if (not defaultFolderExists) 
       then case pbm of
                 Nothing -> return unit
                 Just (Photobooth {defaultprofile}) -> do 
                   mkdir defaultDir
                   downloadFolder baseurl cname defaultprofile defaultDir
       else return unit
         
                                 
    
    ------ Make folders for events --------------
    safeMkdir (normalize "clientprofiles")
    events :: Array PartialEvent <- query getEventsByCname [toSql cname] conn
    eventFolders :: Array String <- filter (flip startsWith "event_") <$> readdir (normalize "clientprofiles")
    let eventIdsFromFolders = (\i -> maybe (-1) id $ safeParseInt i) <<< (drop 6) <$> eventFolders
    let missingEvents = filter (\(PartialEvent {id: Just id}) -> isNothing $ elemIndex id eventIdsFromFolders) events
    liftEff $ log $ "Making event folders for: " <> (joinWith ", " $ (\(PartialEvent {name: name}) -> name) <$> missingEvents)
    
    flip traverse missingEvents (\(PartialEvent ev) -> do 
      id <- maybe (throwError $ error $ "No id for PartialEvent") return ev.id
      let target = mkEventDir id
      let temptarget = mkTempEventDir id -- Writing first to temp folder to have some kind of "transaction"
      rmdirRecur temptarget
      mkdir temptarget
      downloadFolder baseurl cname ev.profile temptarget
      buffers <- getFilesForEvent conn id
      flip traverse buffers (\(NamedBuffer n f) -> overWriteFile (concat [temptarget, n]) f)
      filesToCopy <- readdir temptarget
      copyDir temptarget target
      rmdirRecur temptarget
      liftEff $ log $ "Made folder for event " <> show id
    )
    
   
   
   
   
    
startsWith :: String -> String -> Boolean
startsWith big small = take (length small) big == small
    
data LastUpdated = LastUpdated String
               
instance isForeignLastUpdated :: IsForeign LastUpdated where
  read obj = do
    lastupdatedon <- readProp "lastupdatedon" obj
    return $ LastUpdated lastupdatedon
    
toStr :: LastUpdated -> String
toStr (LastUpdated s) = s

data WrappedId = WrappedId Int
instance isForeignWrappedId :: IsForeign WrappedId where
  read obj = do
    id <- readProp "id" obj
    return $ WrappedId id
    
unwrap :: WrappedId -> Int
unwrap (WrappedId i) = i
    
getLastUpdateEvents :: Query LastUpdated
getLastUpdateEvents = Query $ "select coalesce(max(updatedon), '2010-01-01 00:00:00') as lastupdatedon from events"
                                     
getLastUpdateFiles :: Query LastUpdated
getLastUpdateFiles = Query $ "select coalesce(max(updatedon), '2010-01-01 00:00:00') as lastupdatedon from files"
                                     
getEmptyFileEntries :: Query WrappedId
getEmptyFileEntries = Query $ "select id from files where file is NULL"
                                                          
getEventsByCname :: Query PartialEvent
getEventsByCname = Query $ "select * from events where computername = ?"
                                                       
downloadFolder :: forall eff. String -> String -> String -> String -> Aff (ajax :: AJAX, fs :: FS, buffer :: BUFFER, console :: CONSOLE | eff) Unit
downloadFolder baseurl cname profile target = do
  files <- execEndpoint_ baseurl getProfileFiles (Tuple cname profile) unit
  let makereq fn = {headers: [], content: Nothing, method: GET, url: baseurl ++ "/" ++ fn, username: Nothing, password: Nothing } :: AffjaxRequest Unit
  traverse (\filename -> do {response} :: AffjaxResponse Buffer <- affjax $ makereq filename
                            writeFile (concat [target, basename filename]) response) files
  return unit
                                               
data NamedBuffer = NamedBuffer String Buffer

instance isForeignNamedBuffer :: IsForeign NamedBuffer where
  read obj = do
    file <- readProp "file" obj
    name <- readProp "name" obj
    return $ NamedBuffer name $ unsafeFromForeign file
                                               
getFilesForEvent :: forall eff. Connection -> Int -> Aff (db :: DB | eff) (Array NamedBuffer)
getFilesForEvent c i = query (Query "select file, name from files where eventid = ?") [toSql i] c
                                                          
downloadFileById :: forall eff. String -> Int -> Aff (ajax :: AJAX | eff) Buffer
downloadFileById s i = do
  let req = {headers: [], content: Nothing, method: GET, url: s <> "/api/files/" <> show i, username: Nothing, password: Nothing } :: AffjaxRequest Unit 
  res :: AffjaxResponse Buffer <- affjax req 
  case res of 
       {response} -> return response

saveFile :: forall eff. Connection -> Int -> Buffer -> Aff (db :: DB | eff) Unit
saveFile c i b = execute (Query "update files set file = ? where id = ?") [toSql b, toSql i] c
                                     
defaultDate :: forall m. (MonadError Error m) => Maybe LastUpdated -> Maybe Date -> m Date
defaultDate ms md = maybe (throwError $ error "Failed to parse date - last updated events") return ((join $ fromString <$> (toStr <$> ms)) <|> md)
