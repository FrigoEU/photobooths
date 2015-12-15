module Main where

import Prelude

import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (runAff, Aff())
import Control.Monad.Error.Class (throwError, MonadError)

import Data.Date (now, Date(), Now(), fromStringStrict)
import Data.Array (filterM)
import Data.StrMap (lookup)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (traverse)
--import Control.Apply ((*>))

import Database.AnyDB (DB(), withConnection, Connection())
import Node.Path (normalize, concat)
import Node.FS (FS())
import Node.FS.Aff (readdir, stat)
import Node.FS.Stats (isDirectory)

import Server.Core

import App.Endpoint
import App.DB


port :: Int
port = 8080

main :: forall eff. Eff (now :: Now, console :: CONSOLE, db :: DB, express :: EXPRESS, fs :: FS | eff) Unit
main = do
  dateNow <- now
  --runAff (log <<< show) (const $ log "Initial SQL OK") $ withConnection connectionInfo \conn -> do
      --dropDB conn
      --makeDB conn
      --loadWithDummy conn dateNow
  app <- makeApp
  hostEndpoint app getPhotobooth $ readCname "GetPhotobooth" queryPhotobooth
  hostEndpoint app getPhotobooths allPhotobooths
  hostEndpoint app postPhotobooths newPB
  hostEndpoint app putPhotobooths updatePB
  hostEndpoint app getEvents eventsByCname
  hostEndpoint app postEvents newEvent
  hostEndpoint app putEvents updateEvent
  hostEndpoint app getNewEvents $ readCnameDate "GetNewEvents" queryNewEvents
  hostEndpoint app getProfiles (const allProfiles)
  hostEndpoint app getStatistics allStatistics
  hostEndpoint app getNewFiles $ readCnameDate "GetNewFiles" queryNewFiles
  hostEndpoint app postStatistics (\{body: b} -> withConnection connectionInfo $ 
                                                  \c -> addStatistics c b)
  hostEndpoint app getProfileFiles $ readCnamePname "GetProfileFiles" findProfileFiles
  hostFileUploadEndpoint app attachFile saveFileToDb
  hostFile app "/api/files/:id" getFileById
  hostStatic app "static"
  hostStatic app "profiles"
  listen app port
  log $ "Starting server on " ++ show port


allProfiles :: forall eff. Aff (fs :: FS | eff) (Array (Tuple String (Array String)))
allProfiles = do
  let prof = (normalize "./profiles")
  inProfiles <- readdir prof
  dirsInProfiles <- flip filterM inProfiles (\path -> stat (concat [prof, path]) >>= (return <<< isDirectory))
  flip traverse dirsInProfiles (\dir -> readdir (concat [prof, dir]) >>= (\profiles -> return $ Tuple dir profiles))


-- TODO REFACTOR! -----
readCname :: forall a eff. String -> (Connection -> String -> Aff (db :: DB | eff) a) -> Input Unit -> Aff (db :: DB | eff) a
readCname s f {params} =
  case lookup "cname" params of
       Nothing -> throwError $ error $ s <> ": No computername provided"
       Just cname -> withConnection connectionInfo (\c -> f c cname)

readCnameDate :: forall a b eff. String -> (Connection -> String -> Date -> Aff (db :: DB | eff) a) -> Input b -> Aff (db :: DB | eff) a
readCnameDate s f {params} =
  case lookup "cname" params of
       Nothing -> throwError $ error $ s <> ": No computername provided"
       Just cname -> case lookup "date" params of
                          Nothing -> throwError $ error $ s <> ": No date provided"
                          Just date -> case fromStringStrict date of
                                            Nothing -> throwError $ error $ s <> ": Invalid date provided"
                                            Just d -> withConnection connectionInfo (\c -> f c cname d)
                                            

readCnamePname :: forall a b m. (MonadError Error m) => String -> (String -> String -> m a) -> Input b -> m a
readCnamePname s f {params} = 
  case lookup "cname" params of
       Nothing -> throwError $ error $ s <> ": No computername provided"
       Just cname -> case lookup "pname" params of
                          Nothing -> throwError $ error $ s <> ": No profilename provided"
                          Just pname -> f cname pname
                          



findProfileFiles :: forall eff. String -> String -> Aff (fs :: FS | eff) (Array String)
findProfileFiles cname pname = ((<$>) \path -> concat [cname, pname, path]) <$> readdir (concat ["profiles", cname, pname])

--testFindProfileFiles = runAff (log <<< show) (\s -> traverse log s *> return unit) (findProfileFiles "mycomputername" "profile1")
