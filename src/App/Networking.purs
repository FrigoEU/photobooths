module App.Networking where

import App.Config (WorkerConfig(WorkerConfig), readConfigFile)
import App.DB (queryAllStatistics, upsertSavedFile, upsertPartialEvent, upsertPB, queryPhotobooth, networkingConnectionInfo)
import App.Endpoint (getProfileFiles, execEndpoint_, postStatistics, getNewFiles, getNewEvents, getPhotobooth)
import App.FS (mkEventDir, rmdirRecur, copyDir, overWriteFile, mkTempEventDir, defaultDir, safeMkdir)
import App.Model.Date (toISOString)
import App.Model.Event (PartialEvent(PartialEvent))
import App.Model.Photobooth (Photobooth(Photobooth))
import App.Model.SavedFile (SavedFile(SavedFile))
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind (join)
import Control.Monad.Aff (makeAff, runAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (message, stack, error, Error)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Error.Class (throwError, class MonadError)
import DOM.File.Types (Blob)
import Data.Array (filter, elemIndex)
import Data.Date (fromStringStrict, Now, now, Date, fromString)
import Data.Either (Either(Right, Left), either)
import Data.Foreign (F, toForeign, ForeignError(TypeMismatch), unsafeFromForeign)
import Data.Foreign.Class (read, class IsForeign, readProp)
import Data.Int.Extended (safeParseInt)
import Data.Maybe (Maybe(Nothing, Just), maybe, isNothing)
import Data.Maybe.Unsafe (fromJust)
import Data.String (replace, joinWith, drop)
import Data.String.Extended (startsWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Database.AnyDB (DB, Connection, Query(Query), execute, query, execute_, queryOne, withConnection)
import Database.AnyDB.SqlValue (toSql)
import Database.AnyDB.Transaction (withTransaction)
import Network.HTTP.Affjax (AJAX)
import Node.Buffer (Buffer, BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (unlink, readFile, readdir, mkdir, exists, stat, appendTextFile)
import Node.FS.Stats (Stats(Stats))
import Node.OS (hostname)
import Node.Path (normalize, concat, basename)
import Prelude ((<>), Unit, (<$>), ($), return, pure, bind, show, unit, (++), (<<<), flip, id, negate, not, (>>=), (/=), (>), const)
import Unsafe.Coerce (unsafeCoerce)

logfile :: String
logfile = "networkinglog.txt"

logToFile :: forall eff. String -> Aff ( fs :: FS, now :: Now | eff ) Unit
logToFile s = liftEff now >>= \n -> appendTextFile UTF8 logfile (toISOString n <> " " <> s <> "\n")

main = do
  --let cname = "mycomputername"
  runAff (\e -> let m = show e <> message e <> maybe "" id (stack e)
                 in (log m) *> runAff (\_ -> pure unit) (\_ -> pure unit) (logToFile m)) 
         (const $ log "Everything synced!") 
         $ withConnection networkingConnectionInfo $ \conn -> do
    logToFile "Started networkingscript"
    stat logfile >>= \(Stats {size}) -> if size > 50000000.0 then unlink logfile else pure unit

    fcf <- liftEff $ readConfigFile
    cf <- either (throwError <<< error <<< show) pure fcf
    let baseurl = case cf of 
                       (WorkerConfig {webServiceHost}) -> webServiceHost
    cname <- liftEff $ hostname
    {-- let cname = "snapyo7" --}
    ------ For testing purposes ----- 
    {-- dropDB conn --}
    {-- makeDB conn --}
    
    logToFile "Initial SQL Done"
    
    ------ Sync Photobooth ----------
    oldPb <- queryPhotobooth conn cname
    pbm <- execEndpoint_ baseurl getPhotobooth cname unit
    case pbm of
         Nothing -> throwError $ error $ "No photobooth instance found for: " <> cname
         Just pb -> execute_ (upsertPB pb) conn
    if oldPb /= pbm then rmdirRecur defaultDir else return unit
    logToFile "Synced Photobooth(s)"
    
    ------ Sync Events --------------
    maybeLastUpdatedEvents <- queryOne getLastUpdateEvents [] conn
    let lastUpdatedEvents = readLastUpdated maybeLastUpdatedEvents 
    pes <- execEndpoint_ baseurl getNewEvents (Tuple cname lastUpdatedEvents) unit
    traverse (\(PartialEvent {id: Just id}) -> rmdirRecur $ mkEventDir id) pes -- Event updated: regenerate folder
    withTransaction (\tc -> traverse (\pe -> execute_ (upsertPartialEvent pe) conn) pes) conn
    logToFile "Synced Events"
    
    ------ Sync Files  --------------
    maybeLastUpdatedFiles <- queryOne getLastUpdateFiles [] conn
    let lastUpdatedFiles = readLastUpdated maybeLastUpdatedFiles
    files <- execEndpoint_ baseurl getNewFiles (Tuple cname lastUpdatedFiles) unit
    let eventIdsWithNewFiles = (\(SavedFile {eventId: i}) -> i) <$> files
    traverse (\i -> rmdirRecur $ mkEventDir i) eventIdsWithNewFiles -- File updated: regenerate event folder
    withTransaction (\tc -> traverse (\sf -> execute_ (upsertSavedFile sf) conn) files) conn
    logToFile "Synced Files"
    
    ------ Sync File contents  --------------
    eventids <- query getEmptyFileEntries [] conn
    flip traverse eventids (\(WrappedId id) -> downloadFileById baseurl id >>= (\b -> saveFile conn id b))
    logToFile "Synced File contents"
    
    ------ Sync statistics back to master  --------------
    stats <- queryAllStatistics conn cname
    execEndpoint_ baseurl postStatistics unit stats
    logToFile "Synced statistics"
    
    ------ Make default profile folder -------------
    safeMkdir (normalize "clientprofiles")
    defaultFolderExists <- exists defaultDir
    if (not defaultFolderExists) 
       then case pbm of
                 Nothing -> return unit
                 Just (Photobooth {defaultprofile}) -> do 
                   mkdir defaultDir
                   downloadFolder baseurl cname defaultprofile defaultDir
       else return unit
         
                                 
    
    ------ Make folders for events --------------
    events :: Array PartialEvent <- query getEventsByCname [toSql cname] conn
    eventFolders :: Array String <- filter (flip startsWith "event_") <$> readdir (normalize "clientprofiles")
    let eventIdsFromFolders = (\i -> maybe (-1) id $ safeParseInt i) <<< (drop 6) <$> eventFolders
    let missingEvents = filter (\(PartialEvent {id: Just id}) -> isNothing $ elemIndex id eventIdsFromFolders) events
    logToFile $ "Making event folders for: " <> (joinWith ", " $ (\(PartialEvent {name: name}) -> name) <$> missingEvents)
    
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
      logToFile $ "Made folder for event " <> show id
    )
    
   
   
   
   
    
data LastUpdated = LastUpdated Date
               
instance isForeignLastUpdated :: IsForeign LastUpdated where
  read obj = do
    f <- readProp "lastupdatedon" obj
    lastupdatedon <- maybe (Left $ TypeMismatch "Well formed date" f) Right (fromString ((replace " " "T" f) <> ".000"))
    return $ LastUpdated lastupdatedon
    
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
                                                       
foreign import _downloadFile :: forall e a. String -> String -> (Error -> Eff (ajax :: AJAX | e) Unit) -> (a -> Eff (ajax :: AJAX | e) Unit) -> Eff (ajax :: AJAX | e) Unit

downloadFolder :: forall eff. String -> String -> String -> String -> Aff (ajax :: AJAX, fs :: FS, buffer :: BUFFER, console :: CONSOLE | eff) Unit
downloadFolder baseurl cname profile target = do
  files <- execEndpoint_ baseurl getProfileFiles (Tuple cname profile) unit
  traverse (\filename -> makeAff (_downloadFile (concat [target, basename filename]) (baseurl ++ "/" ++ filename))) files
  return unit
                                               
data NamedBuffer = NamedBuffer String Buffer

blobToBuffer :: Blob -> Buffer
blobToBuffer = unsafeCoerce

instance isForeignNamedBuffer :: IsForeign NamedBuffer where
  read obj = do
    file <- readProp "file" obj
    name <- readProp "name" obj
    return $ NamedBuffer name $ unsafeFromForeign file
                                               
getFilesForEvent :: forall eff. Connection -> Int -> Aff (db :: DB | eff) (Array NamedBuffer)
getFilesForEvent c i = query (Query "select file, name from files where eventid = ?") [toSql i] c
                                                          
downloadFileById :: forall eff. String -> Int -> Aff (ajax :: AJAX, buffer :: BUFFER, fs :: FS, random :: RANDOM | eff) Buffer
downloadFileById s i = do
  ri <- liftEff $ randomInt 0 100000
  let tempfile = "./temp" <> show ri
  makeAff $ _downloadFile tempfile (s <> "/api/files/" <> show i)
  buf <- readFile tempfile
  unlink tempfile
  pure buf

saveFile :: forall eff. Connection -> Int -> Buffer -> Aff (db :: DB | eff) Unit
saveFile c i b = execute (Query "update files set file = ? where id = ?") [toSql b, toSql i] c

readLastUpdated ms = case fromJust ms of
                          LastUpdated d -> d
