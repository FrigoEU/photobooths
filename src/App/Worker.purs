module App.Worker where

import Prelude (Unit, show, (<>), return, ($), bind, unit, (>>=), flip, (+), (<$>), (-), (/=), const, (<<<))

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff) 

import Database.AnyDB (DB(), Connection(), ConnectionInfo(Sqlite3), Query(Query), execute_, queryOne_, withConnection) 

import Node.FS (FS()) 
import Node.FS.Aff (readFile, readdir, unlink, rename, readTextFile) 
import Node.Path (FilePath(), concat, basename) 
import Node.Buffer (BUFFER())
import Node.ChildProcess (CHILD_PROCESS)
import Node.Encoding (Encoding(UTF8))

import App.FS (safeMkdir, overWriteFile, mkEventDir, defaultDir, rmdirRecur)
import App.DB (updateWorkerState, upsertEventStatistic, upsertMonthlyStatistic, getMonthlyStatistic, queryActiveEvent, safeParseInt) 
import App.Model.WorkerState (Active(EventActive, DefaultActive), WorkerState(WorkerState)) 
import App.Model.Event (PartialEvent(PartialEvent)) 
import App.Model.Statistic (EventStatistic(EventStatistic), MonthlyStatistic(MonthlyStatistic), monthToInt) 
import App.Exec (exec)

import Data.Traversable (traverse) 
import Data.Maybe (Maybe(Just, Nothing), maybe) 
import Data.Date (Date(), now) 
import Data.Date.Locale (Locale(), month) 
import Data.Array (length, last, (!!))
import Data.String.Regex (Regex, match, noFlags, regex)


mainPhotosDir :: FilePath
mainPhotosDir = "photos"

historyFolder :: FilePath
historyFolder = "photoshistory"

targetDir :: FilePath
targetDir = "background_images"

printsdir :: FilePath
printsdir = "prints"

workerConnI :: ConnectionInfo
workerConnI = Sqlite3 { filename: "workerdb"
                               , memory: false }

main ::  _
main = runAff (log <<< show) (const $ log "Worker done!") $ withConnection workerConnI \conn -> do
  let cname = "mycomputername"
      
  ------ Move photos into photohistory folder -------------
  safeMkdir historyFolder
  safeMkdir mainPhotosDir
  wsm <- queryOne_ queryWorkerState conn
  ws <- maybe (throwError $ error "No Workerstate found") return wsm
  toCopy <- readdir mainPhotosDir
  let dirToCopyTo = case ws of (WorkerState {active}) -> mkDirForActive active
  safeMkdir dirToCopyTo
  flip traverse toCopy (\f -> rename f (concat [dirToCopyTo, basename f]))
  
  ------ Swap events ----------------------
  dateNow <- liftEff $ now
  evm <- queryActiveEvent conn dateNow 
  newActive <- case evm of
                   Nothing -> return DefaultActive
                   (Just (PartialEvent {id: Nothing})) -> throwError $ error "No ID in active even"
                   (Just (PartialEvent {id: Just i})) -> return $ EventActive i
  case ws of 
       (WorkerState {active}) -> if newActive /= active 
                                    then switchEvents dateNow conn cname active newActive
                                    else return unit
  return unit
  
  
switchEvents :: forall eff. Date -> Connection -> String -> Active -> Active ->
                Aff (fs :: FS, db :: DB, buffer :: BUFFER, locale :: Locale, cp :: CHILD_PROCESS, ex :: EXCEPTION | eff) Unit 
switchEvents dateNow conn cname old new = do
  let oldPhotosFolder = mkDirForActive old
  let newPhotosFolder = mkDirForActive new

  ------ Handling previous event --------
  killPrograms
  ------ Counting photos         --------
  oldPhotos <- readdir oldPhotosFolder
  traverse unlink oldPhotos

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
                   (MonthlyStatistic (m { pictures = m.pictures + length oldPhotos 
                                        , prints = m.prints + printcount}))
         execute_ q conn 
       EventActive i -> do 
         let s = EventStatistic { computername: cname
                                , eventId: i
                                , pictures: length oldPhotos
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

mkDirForActive :: Active -> String
mkDirForActive DefaultActive = concat [mainPhotosDir, "default"]
mkDirForActive (EventActive i) = concat [mainPhotosDir, "event_" <> show i]

