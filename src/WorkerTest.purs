module WorkerTest where

import Prelude (Unit, ($), bind, return, (<>), (==))

import Database.AnyDB (Connection, DB, connect)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error, EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)

import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Date (fromStringStrict, now, Now)
import Data.Tuple (Tuple(Tuple))
import Data.Array (length, (!!))

import Node.Process (PROCESS, chdir)
import Node.OS (OS, hostname)
import Node.FS.Aff (writeTextFile, readTextFile, readdir)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.Path (normalize, concat)
import Node.ChildProcess (CHILD_PROCESS)
import Node.Buffer (fromString)
import Node.Path (concat, normalize)
import Node.Encoding (Encoding(UTF8))

import App.DB (saveFileToDb, newEvent, newPB, queryEvents, allPhotobooths, updateEvent, queryAllStatistics, makeDB, dropDB, networkingConnectionInfo, mainConnectionInfo)
import App.Model.Photobooth (Photobooth(Photobooth))
import App.Model.Event (Event(Event))
import App.Model.Statistic (AllStatistics(AllStatistics), MonthlyStatistic(MonthlyStatistic))
import App.Model.SavedFile (SavedFile)
import App.FS (safeMkdir, rmdirRecur)
import App.Exec (simpleExec)

---------------------------------------------------------------------
-- How to:
-- Build:
-- npm run build
-- npm run compile:networking -> netw.js
-- npm run compile:worker -> work.js
-- npm run compile:workertest -> worktest.js
----------
-- Put worktest.js into the base test folder
-- This test folder should have two subfolders:
-- * klikhut-master
-- * klikhut-slave
--     This one should have work.js and netw.js scripts that you want to test
-- Run this script: "node worktest.js"
-- Check if every log says "OK"
----------------------------------------------------------------------


photosDir :: String
photosDir = normalize "photos"
printsDir :: String
printsDir = normalize "prints"
photosPrintsDir :: String
photosPrintsDir = concat ["photos", "prints"]
backgroundImagesDir :: String
backgroundImagesDir = (normalize "background_images")

type TestEffects = ( db :: DB , fs :: FS , cp :: CHILD_PROCESS , err :: EXCEPTION, os :: OS, process :: PROCESS, console :: CONSOLE, buffer :: BUFFER, now :: Now)

main :: Aff TestEffects Unit
main = do
  mainDB <- connect mainConnectionInfo
  workerDB <- connect networkingConnectionInfo
  cname <- liftEff hostname

  setup mainDB workerDB cname

  liftEff $ chdir "klikhut-master"
  makePbAndEvent mainDB cname
  liftEff $ chdir ".."

  liftEff $ chdir "klikhut-slave"
  runAndCheckSync workerDB cname
  liftEff $ chdir ".."

  liftEff $ chdir "klikhut-slave"
  runWorkerAndCheckDefault
  liftEff $ chdir ".."

  liftEff $ chdir "klikhut-slave"
  addEventData workerDB cname
  liftEff $ chdir ".."

  liftEff $ chdir "klikhut-slave"
  runWorkerAndCheckEvent workerDB cname
  liftEff $ chdir ".."

  liftEff $ chdir "klikhut-slave"
  simpleExec "node netw.js" [] Nothing
  liftEff $ chdir ".."

  liftEff $ chdir "klikhut-master"
  checkStatisticsSync mainDB cname
  liftEff $ chdir ".."

  liftEff $ log "Done!"

setup :: Connection -> Connection -> String -> Aff TestEffects Unit
setup mainDB workerDB cname = do
  dropDB workerDB
  makeDB workerDB

  dropDB mainDB
  makeDB mainDB

  liftEff $ chdir "klikhut-master"
  rmdirRecur cname
  safeMkdir cname
  safeMkdir $ concat [cname, "defaultprofile"]
  safeMkdir $ concat [cname, "eventprofile"]
  liftEff $ chdir ".."

  liftEff $ chdir "klikhut-slave"
  rmdirRecur photosDir
  rmdirRecur printsDir
  safeMkdir photosDir
  safeMkdir printsDir
  safeMkdir photosPrintsDir
  liftEff $ chdir ".."


checkStatisticsSync :: Connection -> String -> Aff TestEffects Unit
checkStatisticsSync mainDB cname = do
  (AllStatistics {eventStatistics, monthlyStatistics}) <- queryAllStatistics mainDB cname
  liftEff $ check (length eventStatistics == 0) "No eventstatistics synced"
  liftEff $ check (length monthlyStatistics == 0) "1 monthlyStatistic synced"

runWorkerAndCheckEvent :: Connection -> String -> Aff TestEffects Unit
runWorkerAndCheckEvent workerDB cname = do
  simpleExec "node work.js" [] Nothing
  (AllStatistics {eventStatistics, monthlyStatistics}) <- queryAllStatistics workerDB cname
  liftEff $ check (length eventStatistics == 0) "No eventstatistics yet"
  (MonthlyStatistic mstat) <- maybe (throwError $ error "No monthly stat found!") return $ monthlyStatistics !! 0
  liftEff $ check (mstat.pictures == 1) "1 picture counted for default"
  liftEff $ check (mstat.prints == 21) "21 prints counted for default"
  photosDirFiles <- readdir (normalize "photos")
  liftEff $ check (photosDirFiles == ["prints"]) "Photos gone from 'photos' folder"
  photosHistoryFiles <- readdir (concat ["photoshistory", "default"])
  liftEff $ check (photosDirFiles == ["prints","mypic.jpg"]) "Photos moved to photoshistory folder"
  activeFiles <- readdir (normalize "background_images")
  liftEff $ check (activeFiles == ["ev1.txt", "ev2.txt"]) "Event Profile active"

addEventData :: Connection -> String -> Aff TestEffects Event
addEventData workerDB cname = do
  writeTextFile UTF8 (concat ["photos", "mypic.jpg"]) "myphoto"
  writeTextFile UTF8 (concat ["photos", "prints", "myprintpic.jpg"]) "myphotoprint"
  safeMkdir (concat ["prints", "000001"])
  writeTextFile UTF8 (concat ["prints", "000001", "DataCollector01.csv"])
                     "\"blablabla\"\n\"12/18/2015 15:26:46.959\",\"21\""
  future <- maybe (throwError $ error "future") return $ fromStringStrict "2020-12-21T01:0&:01.000Z"
  nowDate <- liftEff now
  updateEvent workerDB (Event { id: Just 1
                              , computername: cname
                              , name: "eventname"
                              , datefrom: nowDate -- so it gets started when we run worker next
                              , dateuntil: future
                              , profile: "eventprofile"
                              , files: []})

runWorkerAndCheckDefault :: Aff TestEffects Unit
runWorkerAndCheckDefault = do
  simpleExec "node work.js" [] Nothing
  activeFiles <- readdir backgroundImagesDir
  liftEff $ check (activeFiles == ["def.txt"]) "Default Profile active"

runAndCheckSync :: Connection -> String -> Aff TestEffects Unit
runAndCheckSync workerDB cname = do
  simpleExec "node netw.js" [] Nothing
  pbs <- allPhotobooths workerDB
  liftEff $ check (length pbs == 1) "Photobooth syncing"
  evs <- queryEvents workerDB cname
  liftEff $ check (length evs == 1) "Events syncing"
  defFiles <- readdir (concat ["clientprofiles", "default"])
  liftEff $ check (defFiles == ["def.txt"]) "Default Profile Folder making"
  evFiles <- readdir (concat ["clientprofiles", "event_1"])
  liftEff $ check (defFiles == ["ev1.txt", "ev2.txt"]) "Event Profile Folder making init"
  ev2File <- readTextFile UTF8 (concat ["clientprofiles", "ev2.txt"]) 
  liftEff $ check (ev2File == "changed!") "Event Profile Folder making overwrite"

check :: Boolean -> String -> Eff TestEffects Unit
check bool text = if bool then (log $ "OK - " <> text) 
                          else (log $ "ERROR - " <> text)

makePbAndEvent :: Connection -> String -> Aff TestEffects SavedFile
makePbAndEvent mainDB cname = do
  future <- maybe (throwError $ error "future") return $ fromStringStrict "2020-12-21T01:0&:01.000Z"
  writeTextFile UTF8 (concat [cname, "defaultprofile", "def.txt"]) "def"
  writeTextFile UTF8 (concat [cname, "eventprofile",   "ev1.txt"]) "ev1"
  writeTextFile UTF8 (concat [cname, "eventprofile",   "ev2.txt"]) "ev2"
  newPB mainDB (Photobooth { id: Nothing
                           , computername: cname
                           , alias: "pipi"
                           , defaultprofile: "defaultprofile"})
  newEvent mainDB (Event { id: Nothing
                         , computername: cname
                         , name: "eventname"
                         , datefrom: future
                         , dateuntil: future
                         , profile: "eventprofile"
                         , files: []})
  filebuf <- liftEff $ (fromString "changed!" UTF8)
  saveFileToDb mainDB (Tuple 1 "ev1.txt") filebuf
