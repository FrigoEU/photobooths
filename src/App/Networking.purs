module App.Networking where

import Prelude

import Database.AnyDB.SqlValue (toSql)
import Database.AnyDB
import Database.AnyDB.Transaction

import Data.Maybe
import Data.Date
import Data.Tuple
import Data.String (joinWith, take, drop)
import Data.Traversable
import Data.Array (filter, elemIndex)
import Data.Foreign
import Data.Foreign.Class
import DOM.File.Types(Blob())

import Node.Buffer (Buffer(), BUFFER())
import Node.FS.Aff (readdir, stat, mkdir, writeFile, rename)
import Node.FS.Stats (isDirectory)
import Node.FS (FS())
import Node.Path (normalize, concat)

import Unsafe.Coerce

import Network.HTTP.Affjax 
import Network.HTTP.Method (Method(..))

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class
import Control.Monad.Aff (runAff)
import Control.Monad.Error.Class (throwError, MonadError)
import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Aff (Aff())
import Control.Alt
import Control.Bind (join)
import Control.Apply((*>))

import App.DB
import App.Endpoint
import App.Model.Event

networkingConnectionInfo :: ConnectionInfo
networkingConnectionInfo = Sqlite3
  { filename: "networkingdb"
  , memory: false }
  
main = do
  let cname = "mycomputername"
  let baseurl = "http://localhost:8080"
  runAff (log <<< show) (const $ log "Everything synced!") $ withConnection networkingConnectionInfo $ \conn -> do
    --dropDB conn
    --makeDB conn
    liftEff $ log "Initial SQL Done"
    
    pbm <- execEndpoint_ baseurl getPhotobooth cname unit
    case pbm of
         Nothing -> throwError $ error $ "No photobooth instance found for: " <> cname
         Just pb -> execute_ (upsertPB pb) conn
    liftEff $ log "Synced Photobooth(s)"
    
    maybeLastUpdatedEvents <- queryOne getLastUpdateEvents [] conn
    lastUpdatedEvents <- defaultDate maybeLastUpdatedEvents (fromStringStrict "2010-01-01T00:00:00.000Z")
    pes <- execEndpoint_ baseurl getNewEvents (Tuple cname lastUpdatedEvents) unit
    withTransaction (\tc -> traverse (\pe -> execute_ (upsertPartialEvent pe) conn) pes) conn
    liftEff $ log "Synced Events"
    
    maybeLastUpdatedFiles <- queryOne getLastUpdateFiles [] conn
    lastUpdatedFiles <- defaultDate maybeLastUpdatedFiles (fromStringStrict "2010-01-01T00:00:00.000Z")
    files <- execEndpoint_ baseurl getNewFiles (Tuple cname lastUpdatedFiles) unit
    withTransaction (\tc -> traverse (\sf -> execute_ (upsertSavedFile sf) conn) files) conn
    liftEff $ log "Synced Files"
    
    eventids <- query getEmptyFileEntries [] conn
    flip traverse eventids (\(WrappedId id) -> downloadFileById baseurl id >>= (\b -> saveFile conn id b))
    liftEff $ log "Synced File contents"
    
    stats <- queryAllStatistics conn cname
    execEndpoint_ baseurl postStatistics unit stats
    liftEff $ log "Synced statistics"
    
    events :: Array PartialEvent <- query getEventsByCname [toSql cname] conn
    eventFolders :: Array String <- (filter (\s -> take 6 s == "event_")) <$> readdir (normalize "./clientprofiles")
    let eventIdsFromFolders = (\i -> maybe (-1) id $ safeParseInt i) <<< (drop 6) <$> eventFolders
    let missingEvents = filter (\(PartialEvent {id: Just id}) -> isNothing $ elemIndex id eventIdsFromFolders) events
    liftEff $ log $ "Making event folders for: " <> (joinWith ", " $ show <<< (\(PartialEvent {id: Just id}) -> id) <$> missingEvents)
    
    flip traverse missingEvents (\(PartialEvent ev) -> do 
      id <- maybe (throwError $ error $ "No id for PartialEvent") return ev.id
      let target = "event_" <> show id
      let temptarget = target <> "tmp"
      mkdir (concat ["clientprofiles", temptarget])    
      downloadFolder cname ev.profile temptarget
      buffers <- getFilesForEvent conn id
      flip traverse buffers (\(NamedBuffer n f) -> writeFile (concat [temptarget, n]) f) -- Test if this OVERWRITES!!
      rename temptarget target -- Test if this renames folders!
      liftEff $ log $ "Made folder for event " <> show id
      )
    
   
   
   
   
    -- *TODO*: Delete event folder when downloading/updating events AND files 
    
    
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
                                                       
downloadFolder :: forall eff. String -> String -> String -> Aff (ajax :: AJAX, fs :: FS, buffer :: BUFFER | eff) Unit
downloadFolder cname profile target = do
  files <- execEndpoint getProfileFiles (Tuple cname profile) unit
  let makereq fn = {headers: [], content: Nothing, method: GET, url: fn, username: Nothing, password: Nothing } :: AffjaxRequest Unit
  traverse (\filename -> do {response} :: AffjaxResponse Buffer <- affjax $ makereq filename
                            writeFile (concat [target, filename]) response) files
  return unit
                                               
data NamedBuffer = NamedBuffer String Buffer

instance isForeignNamedBuffer :: IsForeign NamedBuffer where
  read obj = do
    file <- readProp "file" obj
    name <- readProp "name" obj
    return $ NamedBuffer name $ unsafeFromForeign file
                                               
getFilesForEvent :: forall eff. Connection -> Int -> Aff (db :: DB | eff) (Array NamedBuffer)
getFilesForEvent c i = query (Query "select file from files where eventid = ?") [toSql i] c
                                                          
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
