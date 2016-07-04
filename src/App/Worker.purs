module App.Worker where

import App.Config (WorkerConfig(WorkerConfig), readConfigFile)
import App.DB (networkingConnectionInfo, updateWorkerState, upsertEventStatistic, upsertMonthlyStatistic, getMonthlyStatistic, queryActiveEvent)
import App.FS (safeMkdir, overWriteFile, mkEventDir, defaultDir, rmdirRecur)
import App.Model.Date (toISOString)
import App.Model.Event (PartialEvent(PartialEvent))
import App.Model.Statistic (EventStatistic(EventStatistic), MonthlyStatistic(MonthlyStatistic), monthToInt)
import App.Model.WorkerState (Active(EventActive, DefaultActive), WorkerState(WorkerState))
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Data.Array (length, last, (!!))
import Data.Date (Date, Now, now)
import Data.Date.Locale (Locale, month)
import Data.Either (either)
import Data.Function (runFn0)
import Data.Int.Extended (safeParseInt)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String.Regex (Regex, match, noFlags, regex)
import Data.Traversable (traverse)
import Database.AnyDB (DB, Connection, ConnectionInfo, Query(Query), execute_, queryOne_, withConnection)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (unlink, appendTextFile, writeFile, readFile, stat, exists, readTextFile, readdir)
import Node.FS.Stats (Stats(Stats))
import Node.OS (OS, hostname)
import Node.Path (FilePath, concat, basename)
import Node.Process (PROCESS)
import Prelude ((>), pure, Unit, show, (<>), (>>=), unit, return, not, (&&), ($), bind, flip, (+), (<$>), (-), (/=), (<<<), const)

mainPhotosDir :: FilePath
mainPhotosDir = "photos"

mainPhotosPrintsDir :: FilePath
mainPhotosPrintsDir = concat ["photos", "prints"]

historyFolder :: FilePath
historyFolder = "photoshistory"

targetDir :: FilePath
targetDir = "background_images"

printsdir :: FilePath
printsdir = "prints"

workerConnI :: ConnectionInfo
workerConnI = networkingConnectionInfo

logfile :: String
logfile = "workerlog.txt"

logToFile :: forall eff. String -> Aff ( fs :: FS, now :: Now | eff ) Unit
logToFile s = liftEff now >>= \n -> appendTextFile UTF8 logfile (toISOString n <> " " <> s <> "\n")

main :: forall t350. Eff ( console :: CONSOLE , db :: DB , fs :: FS , buffer :: BUFFER , locale :: Locale , cp :: CHILD_PROCESS , err :: EXCEPTION, os :: OS, now :: Now, process :: PROCESS | t350 ) Unit
main = runAff (log <<< show) (const $ log "Worker done!") $ withConnection workerConnI \conn -> do
  logToFile "Started Worker Script"
  stat logfile >>= \(Stats {size}) -> if size > 50000000.0 then unlink logfile else pure unit
  cname <- liftEff $ hostname
  fcf <- liftEff $ readConfigFile
  cf <- either (throwError <<< error <<< show) pure fcf
      
  ------ Move photos into photohistory folder -------------
  safeMkdir historyFolder
  safeMkdir mainPhotosDir
  safeMkdir mainPhotosPrintsDir
  mws <- queryOne_ queryWorkerState conn
  ws <- maybe (startup conn cf >>= \_ -> pure $ WorkerState {active: DefaultActive}) return mws
  toCopy <- readdir mainPhotosDir
  printsToCopy <- readdir mainPhotosPrintsDir
  let dirToCopyTo = case ws of (WorkerState {active}) -> mkHistoryDirForActive active
  let printsDirToCopyTo = concat [dirToCopyTo, "prints"]
  safeMkdir dirToCopyTo
  safeMkdir printsDirToCopyTo
  flip traverse toCopy       (\f -> safeCopyFile (concat [mainPhotosDir, f])       (concat [dirToCopyTo,       basename f]))
  flip traverse printsToCopy (\f -> safeCopyFile (concat [mainPhotosPrintsDir, f]) (concat [printsDirToCopyTo, basename f]))
  
  ------ Swap events ----------------------
  dateNow <- liftEff $ now
  evm <- queryActiveEvent conn dateNow 
  newActive <- case evm of
                   Nothing -> return DefaultActive
                   (Just (PartialEvent {id: Nothing})) -> throwError $ error "No ID in active event"
                   (Just (PartialEvent {id: Just i})) -> return $ EventActive i
  case ws of 
       (WorkerState {active}) -> if newActive /= active 
                                    then switchEvents dateNow conn cname active newActive cf
                                    else return unit
  return unit
  
  
switchEvents :: forall eff. Date -> Connection -> String -> Active -> Active -> WorkerConfig ->
                Aff (fs :: FS, db :: DB, buffer :: BUFFER, locale :: Locale, cp :: CHILD_PROCESS, err :: EXCEPTION, console :: CONSOLE, now :: Now | eff) Unit 
switchEvents dateNow conn cname old new cf = do
  logToFile $ "Swapping from " <> show old <> " to " <> show new
  let oldPhotosFolder = mkHistoryDirForActive old
  let newPhotosFolder = mkHistoryDirForActive new

  ------ Handling previous event --------
  killPrograms cf

  ------ Counting photos         --------
  oldPhotos <- readdir oldPhotosFolder
  let amountOfPhotosTakenInOld = length oldPhotos - 1 -- Minus one so we don't count the "prints" directory

  ------ Cleaning up main photos --------
  rmdirRecur mainPhotosDir
  safeMkdir mainPhotosDir
  safeMkdir (concat [mainPhotosDir, "prints"])

  ------ Counting prints         --------
  printsfolders <- readdir printsdir
  let lastprintsdir = printsfolders !! (length printsfolders - 1)
  let secondtolastprintsfolders = printsfolders !! (length printsfolders - 2)
  secondtolastcount <- maybe (return 0) (\d -> readPrintCount (concat [printsdir, d])) secondtolastprintsfolders
  printcount <- case lastprintsdir of
                     Nothing -> throwError $ error $ "No print logs found"
                     Just d -> readPrintCount (concat [printsdir, d]) >>= \c -> return $ c - secondtolastcount
 
  ------ Upserting stats table   --------
  monthNow <- liftEff (monthToInt <$> month dateNow)
  case old of
       DefaultActive -> do 
         MonthlyStatistic m <- getMonthlyStatistic cname conn monthNow
         let q = upsertMonthlyStatistic
                   (MonthlyStatistic (m { pictures = m.pictures + amountOfPhotosTakenInOld
                                        , prints = m.prints + printcount}))
         execute_ q conn 
       EventActive i -> do 
         let s = EventStatistic { computername: cname
                                , eventId: i
                                , pictures: amountOfPhotosTakenInOld
                                , prints: printcount}
         let q = upsertEventStatistic s
         execute_ q conn
  maybe (return unit) rmdirRecur secondtolastprintsfolders

  ----- Handling new event ---------------
  let sourcedir = case new of 
                       DefaultActive -> defaultDir
                       EventActive i -> mkEventDir i
  sourcefiles <- readdir sourcedir
  readdir targetDir >>= traverse (\f -> unlink (concat [targetDir, basename f]))
  flip traverse sourcefiles (\f -> readFile (concat [sourcedir, f]) >>= overWriteFile (concat [targetDir, basename f]))
  startPrograms cf

  safeMkdir newPhotosFolder
  safeMkdir $ concat [newPhotosFolder, "prints"]
  updateWorkerState conn $ WorkerState {active: new}
              
  return unit
  
  
startup :: forall e. Connection -> WorkerConfig -> 
           Aff ( fs :: FS , buffer :: BUFFER , cp :: CHILD_PROCESS , err :: EXCEPTION , db :: DB | e ) Unit
startup conn cf = do
  safeMkdir $ targetDir
  sourcefiles <- readdir defaultDir
  flip traverse sourcefiles (\f -> readFile (concat [defaultDir, f]) >>= overWriteFile (concat [targetDir, basename f]))
  startPrograms cf

  let newPhotosFolder = mkHistoryDirForActive DefaultActive

  safeMkdir newPhotosFolder
  safeMkdir $ concat [newPhotosFolder, "prints"]
  updateWorkerState conn $ WorkerState {active: DefaultActive}
              
  return unit
  
  
lastNumber :: Regex
lastNumber = regex "\"(\\d+)\"$" noFlags
  
readPrintCount :: forall eff. FilePath -> Aff (fs :: FS | eff) Int
readPrintCount fp = do
  files <- readdir fp
  lastfile <- maybe (throwError $ error $ "No printer log files found in " <> fp) return (last files)
  contents <- readTextFile UTF8 (concat [fp, lastfile])
  case match lastNumber contents of
       Nothing -> throwError $ error $ "No number found in print log at " <> lastfile
       Just matches -> 
         case matches !! 1 of
              Nothing -> throwError $ error $ "No number match found in print log at " <> lastfile
              Just Nothing -> throwError $ error $ "No num match found in print log at " <>lastfile
              Just (Just n) -> maybe 
                                (throwError $ error $ "Couldn't parse " <> n <> " in " <> lastfile)
                                return 
                                (safeParseInt n)

  
killPrograms :: forall t78. WorkerConfig -> Aff ( cp :: CHILD_PROCESS , err :: EXCEPTION , buffer :: BUFFER | t78 ) Unit
killPrograms (WorkerConfig {photoProgramExe}) = do
  apathize $ simpleExec ("taskkill") Nothing ["/F", "/IM", photoProgramExe] Nothing
  apathize $ simpleExecStr "logman stop Prints" Nothing Nothing
  pure unit

startPrograms :: forall t88. WorkerConfig -> Aff ( cp :: CHILD_PROCESS , err :: EXCEPTION , buffer :: BUFFER | t88 ) Unit
startPrograms (WorkerConfig {photoProgramFullPath, photoProgramExe}) = do
  apathize $ simpleExecStr "logman start Prints" Nothing Nothing
  apathize $ simpleExec ("taskkill") Nothing ["/F", "/IM", photoProgramExe] Nothing
  simpleExecStr ("start " <> photoProgramFullPath) Nothing Nothing
  pure unit

queryWorkerState :: Query WorkerState
queryWorkerState = Query "select * from WORKERSTATE"

safeCopyFile :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER, console :: CONSOLE | eff) Unit
safeCopyFile start end = do 
  endExists <- exists end
  startIsFile <- stat start >>= \(Stats {isFile}) -> return $ runFn0 isFile
  if not endExists && startIsFile then copyFile start end
                                  else return unit

copyFile :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyFile start end = readFile start >>= writeFile end

mkHistoryDirForActive :: Active -> String
mkHistoryDirForActive DefaultActive = concat [historyFolder, "default"]
mkHistoryDirForActive (EventActive i) = concat [historyFolder, "event_" <> show i]


