module App.Worker where

import Prelude (Unit, show, (<>), (>>=), unit, return, not, (&&), ($), bind, flip, (+), (<$>), (-), map, (/=), const, (<<<))

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff) 
import Control.Monad.Eff (Eff)

import Database.AnyDB (DB(), Connection(), ConnectionInfo(Sqlite3), Query(Query), execute_, queryOne_, withConnection) 


import Node.FS (FS()) 
import Node.FS.Aff (writeFile, readFile, stat, exists, readTextFile, readdir, unlink)
import Node.FS.Stats (Stats(Stats))
import Node.Path (FilePath(), concat, basename) 
import Node.Buffer (BUFFER())
import Node.ChildProcess (CHILD_PROCESS)
import Node.Encoding (Encoding(UTF8))
import Node.OS (OS, hostname)
import Node.Process (PROCESS)

import App.FS (safeMkdir, overWriteFile, mkEventDir, defaultDir, rmdirRecur)
import App.DB (updateWorkerState, upsertEventStatistic, upsertMonthlyStatistic, getMonthlyStatistic, queryActiveEvent, safeParseInt) 
import App.Model.WorkerState (Active(EventActive, DefaultActive), WorkerState(WorkerState)) 
import App.Model.Event (PartialEvent(PartialEvent)) 
import App.Model.Statistic (EventStatistic(EventStatistic), MonthlyStatistic(MonthlyStatistic), monthToInt) 
import App.Exec (exec)
import App.Config (WorkerConfig(WorkerConfig), readConfigFile)

import Data.Traversable (traverse) 
import Data.Maybe (Maybe(Just, Nothing), maybe) 
import Data.Date (Date, Now, now)
import Data.Date.Locale (Locale(), month) 
import Data.Array (length, last, (!!))
import Data.String.Regex (Regex, match, noFlags, regex)
import Data.Function (runFn0)
import Data.Either (either)

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
workerConnI = Sqlite3 { filename: "workerdb"
                               , memory: false }

main :: forall t350. Eff ( console :: CONSOLE , db :: DB , fs :: FS , buffer :: BUFFER , locale :: Locale , cp :: CHILD_PROCESS , err :: EXCEPTION, os :: OS, now :: Now, process :: PROCESS | t350 ) Unit
main = runAff (log <<< show) (const $ log "Worker done!") $ withConnection workerConnI \conn -> do
  --let cname = "mycomputername"
  cname <- liftEff $ hostname
  fcf <- liftEff $ readConfigFile
  photoProgramPath <- either (throwError <<< error <<< show) 
                             (\(WorkerConfig {photoProgramPath}) -> return photoProgramPath) fcf
      
  ------ Move photos into photohistory folder -------------
  safeMkdir historyFolder
  safeMkdir mainPhotosDir
  safeMkdir mainPhotosPrintsDir
  mws <- queryOne_ queryWorkerState conn
  ws <- maybe (throwError $ error "No Workerstate found") return mws
  toCopy <- readdir mainPhotosDir
  printsToCopy <- readdir mainPhotosPrintsDir
  let dirToCopyTo = case ws of (WorkerState {active}) -> mkDirForActive active
  let printsDirToCopyTo = concat [dirToCopyTo, "prints"]
  safeMkdir dirToCopyTo
  safeMkdir printsDirToCopyTo
  flip traverse toCopy       (\f -> safeCopyFile f (concat [dirToCopyTo,       basename f]))
  flip traverse printsToCopy (\f -> safeCopyFile f (concat [printsDirToCopyTo, basename f]))
  
  ------ Swap events ----------------------
  dateNow <- liftEff $ now
  evm <- queryActiveEvent conn dateNow 
  newActive <- case evm of
                   Nothing -> return DefaultActive
                   (Just (PartialEvent {id: Nothing})) -> throwError $ error "No ID in active event"
                   (Just (PartialEvent {id: Just i})) -> return $ EventActive i
  case ws of 
       (WorkerState {active}) -> if newActive /= active 
                                    then switchEvents dateNow conn cname active newActive
                                    else return unit
  return unit
  
  
switchEvents :: forall eff. Date -> Connection -> String -> Active -> Active ->
                Aff (fs :: FS, db :: DB, buffer :: BUFFER, locale :: Locale, cp :: CHILD_PROCESS, err :: EXCEPTION | eff) Unit 
switchEvents dateNow conn cname old new = do
  let oldPhotosFolder = mkDirForActive old
  let newPhotosFolder = mkDirForActive new

  ------ Handling previous event --------
  killPrograms

  ------ Counting photos         --------
  oldPhotos <- readdir oldPhotosFolder
  let amountOfPhotosTakenInOld = length oldPhotos - 1 -- Minus one so we don't count the "prints" directory

  ------ Cleaning up main photos --------
  photosInMain       <- map (\f -> concat [mainPhotosDir,       basename f]) <$> readdir mainPhotosDir
  photosInMainPrints <- map (\f -> concat [mainPhotosPrintsDir, basename f]) <$> readdir mainPhotosPrintsDir
  traverse unlink photosInMain
  traverse unlink photosInMainPrints

  ------ Counting prints         --------
  printsfolders <- readdir printsdir
  let lastphotosdir = printsfolders !! (length printsfolders - 1)
  let secondtolastphotosdir = printsfolders !! (length printsfolders - 2)
  secondtolastcount <- maybe (return 0) readPrintCount secondtolastphotosdir 
  printcount <- case lastphotosdir of
                     Nothing -> throwError $ error $ "No print logs found"
                     Just d -> readPrintCount d >>= \c -> return $ c - secondtolastcount
 
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
  maybe (return unit) rmdirRecur secondtolastphotosdir

  ----- Handling new event ---------------
  sourcefiles <- case new of 
                      DefaultActive -> readdir defaultDir
                      EventActive i -> readdir $ mkEventDir i
  flip traverse sourcefiles (\f -> readFile f >>= overWriteFile (concat [targetDir, basename f]))
  startPrograms

  safeMkdir newPhotosFolder
  updateWorkerState conn $ WorkerState {active: new}
              
  return unit
  
  
  
  
lastNumber :: Regex
lastNumber = regex "\"(\\d+)\"$" noFlags
  
readPrintCount :: forall eff. FilePath -> Aff (fs :: FS | eff) Int
readPrintCount fp = do
  files <- readdir fp
  lastfile <- maybe (throwError $ error $ "No printer log files found in " <> fp) return (last files)
  contents <- readTextFile UTF8 lastfile
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

  
killPrograms :: forall eff. Aff (cp :: CHILD_PROCESS, ex :: EXCEPTION | eff) Unit
killPrograms = exec "logman stop Prints" [] Nothing

startPrograms :: forall eff. Aff (cp :: CHILD_PROCESS, ex :: EXCEPTION | eff) Unit
startPrograms = exec "logman start Prints" [] Nothing

queryWorkerState :: Query WorkerState
queryWorkerState = Query "select * from WORKERSTATE"

safeCopyFile :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
safeCopyFile start end = do 
  endExists <- exists end
  startIsFile <- stat start >>= \(Stats {isFile}) -> return $ runFn0 isFile
  if not endExists && startIsFile then copyFile start end
                                  else return unit

copyFile :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyFile start end = readFile start >>= writeFile end

mkDirForActive :: Active -> String
mkDirForActive DefaultActive = concat [mainPhotosDir, "default"]
mkDirForActive (EventActive i) = concat [mainPhotosDir, "event_" <> show i]

