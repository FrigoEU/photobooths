module App.Worker where

import Prelude (Unit, bind, show, (<>), unit, return, ($), (>>=), flip, (+), (<$>), (/=), const, (<<<)) 

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff) 

import Database.AnyDB (DB, Connection, ConnectionInfo(Sqlite3), Query(Query), execute_, queryOne_, withConnection) 

import Node.FS (FS) 
import Node.FS.Aff (readFile, readdir, unlink, rename) 
import Node.Path (FilePath, concat, basename) 
import Node.Buffer (BUFFER())

import App.FS (safeMkdir, overWriteFile, mkEventDir, defaultDir) 
import App.DB (updateWorkerState, upsertEventStatistic, upsertMonthlyStatistic, getMonthlyStatistic, queryActiveEvent) 
import App.Model.WorkerState (Active(EventActive, DefaultActive), WorkerState(WorkerState)) 
import App.Model.Event (PartialEvent(PartialEvent)) 
import App.Model.Statistic (EventStatistic(EventStatistic), MonthlyStatistic(MonthlyStatistic), monthToInt) 

import Data.Traversable (traverse) 
import Data.Maybe (Maybe(Just, Nothing), maybe) 
import Data.Date (Date, now) 
import Data.Date.Locale (Locale, month) 
import Data.Array (length)


mainPhotosDir :: FilePath
mainPhotosDir = "photos"

historyFolder :: FilePath
historyFolder = "photoshistory"

targetDir :: FilePath
targetDir = "background_images"

workerConnI :: ConnectionInfo
workerConnI = Sqlite3 { filename: "workerdb"
                               , memory: false }

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
                Aff (fs :: FS, db :: DB, buffer :: BUFFER, locale :: Locale | eff) Unit 
switchEvents dateNow conn cname old new = do
  let oldPhotosFolder = mkDirForActive old
  let newPhotosFolder = mkDirForActive new
  oldPhotos <- readdir oldPhotosFolder
  traverse unlink oldPhotos
  monthNow <- liftEff (monthToInt <$> month dateNow)
  
  ------ Handling previous event --------
  case old of
       DefaultActive -> do 
         MonthlyStatistic m <- getMonthlyStatistic cname conn monthNow
         let q = upsertMonthlyStatistic
                   (MonthlyStatistic (m {pictures = m.pictures + length oldPhotos}))
         execute_ q conn 
       EventActive i -> do 
         let s = EventStatistic { computername: cname
                                , eventId: i
                                , pictures: length oldPhotos
                                , prints: 0}
         let q = upsertEventStatistic s
         execute_ q conn
         
  ----- Handling new event ---------------
  killPrograms
  sourcefiles <- case new of 
                      DefaultActive -> readdir defaultDir
                      EventActive i -> readdir $ mkEventDir i
  flip traverse sourcefiles (\f -> readFile f >>= overWriteFile (concat [targetDir, basename f]))
  startPrograms
  safeMkdir newPhotosFolder
  updateWorkerState conn $ WorkerState {active: new}
              
  return unit
  
killPrograms :: forall eff. Aff eff Unit
killPrograms = return unit

startPrograms :: forall eff. Aff eff Unit
startPrograms = return unit

queryWorkerState :: Query WorkerState
queryWorkerState = Query "select * from WORKERSTATE"

mkDirForActive :: Active -> String
mkDirForActive DefaultActive = concat [mainPhotosDir, "default"]
mkDirForActive (EventActive i) = concat [mainPhotosDir, "event_" <> show i]

