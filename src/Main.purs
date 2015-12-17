module Main where

import Prelude

import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (Aff())
import Control.Monad.Error.Class (throwError, MonadError)

import Data.Date (now, Date(), Now(), fromStringStrict)
import Data.Array (filterM)
import Data.StrMap (lookup)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Either
--import Control.Apply ((*>))

import Database.AnyDB (DB(), withConnection, Connection(), ConnectionInfo(..))
import Node.Path (normalize, concat)
import Node.FS (FS())
import Node.FS.Aff (readdir, stat)
import Node.FS.Stats (isDirectory)

import Server.Core 

import App.Endpoint
import App.DB


port :: Int
port = 8080

connectionInfo :: ConnectionInfo
connectionInfo = Sqlite3
  { filename: "klikhutdb"
  , memory: false }

withServerConn :: forall a eff. (Connection -> Aff (db :: DB | eff) a) -> Aff (db :: DB | eff) a 
withServerConn = withConnection connectionInfo

main :: forall eff. Eff (now :: Now, console :: CONSOLE, db :: DB, express :: EXPRESS, fs :: FS | eff) Unit
main = do
  dateNow <- now
  --runAff (log <<< show) (const $ log "Initial SQL OK") $ withConnection connectionInfo \conn -> do
      --dropDB conn
      --makeDB conn
      --loadWithDummy conn dateNow
  app <- makeApp
  hostEndpoint app getPhotobooth $ readParams1 "cname" "GetPhotobooth" \c -> withServerConn \conn -> queryPhotobooth conn c
  hostEndpoint app getPhotobooths (const $ withServerConn allPhotobooths)
  hostEndpoint app postPhotobooths \inp -> withServerConn \conn -> newPB conn inp
  hostEndpoint app putPhotobooths \inp -> withServerConn \conn -> updatePB conn inp
  hostEndpoint app getEvents $ readParams1 "cname" "GetEvents" \c -> withServerConn \conn -> queryEvents conn c
  hostEndpoint app postEvents \inp -> withServerConn \conn -> newEvent conn inp
  hostEndpoint app putEvents \inp -> withServerConn \conn -> updateEvent conn inp
  hostEndpoint app getNewEvents $ readCnameDateParams "GetNewEvents" \c d -> withServerConn \conn -> queryNewEvents conn c d
  hostEndpoint app getProfiles (const allProfiles)
  hostEndpoint app getStatistics $ readParams1 "cname" "AllStatistics" \c -> withServerConn \conn -> queryAllStatistics conn c
  hostEndpoint app getNewFiles $ readCnameDateParams "GetNewFiles" \c d -> withServerConn \conn -> queryNewFiles conn c d
  hostEndpoint app postStatistics \{body} -> withServerConn \c -> addStatistics c body
  hostEndpoint app getProfileFiles $ readParams2 "cname" "pname" "GetProfileFiles" findProfileFiles
  hostFileUploadEndpoint app attachFile $ \p -> readParams2 "eventid" "name" "AttachFile" (\e n -> safeParseIntE "AttachFile" e >>= \eventId -> withServerConn \conn -> saveFileToDb conn eventId n p) p
  hostFile app "/api/files/:id" $ readParams1 "id" "GetFile" \stri -> safeParseIntE "GetFile" stri >>= \id -> withServerConn \conn -> getFileById conn id
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

readCnameDateParams :: forall a b m. (MonadError Error m) => String -> (String -> Date -> m a) -> Input b -> m a
readCnameDateParams label f i = readParams2 "cname" "date" label (\cname d -> fromStringStrictE label d >>= (\date -> f cname date)) i

readParams1 :: forall a m. (MonadError Error m) => String -> String -> (String -> m a) -> Input Unit -> m a
readParams1 key label f {params} =
  maybe (throwError $ error (label <> ": No " <> key <> " provided")) f (lookup key params)

readParams2 :: forall a b m. (MonadError Error m) => String -> String -> String -> (String -> String -> m a) -> Input b -> m a
readParams2 key1 key2 s f {params} = 
  either (\key -> throwError $ error $ s <> ": No" <> key <> " provided") (\(Tuple a b) -> f a b )
    (do e1 <- maybe (Left key1) Right $ lookup key1 params
        e2 <- maybe (Left key2) Right $ lookup key2 params
        return $ Tuple e1 e2)
  
fromStringStrictE :: forall m. (MonadError Error m) => String -> String -> m Date
fromStringStrictE label s = maybe (throwError $ error $ label <> ": Invalid date provided") return (fromStringStrict s)

safeParseIntE :: forall m. (MonadError Error m) => String -> String -> m Int
safeParseIntE label s = maybe (throwError $ error $ label <> ": Invalid int provided") return (safeParseInt s)
  
findProfileFiles :: forall eff. String -> String -> Aff (fs :: FS | eff) (Array String)
findProfileFiles cname pname = ((<$>) \path -> concat [cname, pname, path]) <$> readdir (concat ["profiles", cname, pname])

--testFindProfileFiles = runAff (log <<< show) (\s -> traverse log s *> return unit) (findProfileFiles "mycomputername" "profile1")
