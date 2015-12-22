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
  hostEndpoint app getPhotobooth $ readParams1 "GetPhotobooth" "cname"  
                                     \c -> withServerConn \conn -> queryPhotobooth conn c
  hostEndpoint app getPhotobooths (const $ withServerConn allPhotobooths)
  hostEndpoint app postPhotobooths \{body} -> withServerConn \conn -> newPB conn body
  hostEndpoint app putPhotobooths \{body} -> withServerConn \conn -> updatePB conn body
  hostEndpoint app getEvents $ readParams1 "GetEvents" "cname" 
                                 \c -> withServerConn \conn -> queryEvents conn c
  hostEndpoint app postEvents \{body} -> withServerConn \conn -> newEvent conn body
  hostEndpoint app putEvents \{body} -> withServerConn \conn -> updateEvent conn body
  hostEndpoint app getNewEvents $ readCnameDateParams "GetNewEvents" 
                                    \c d -> withServerConn \conn -> queryNewEvents conn c d
  hostEndpoint app getProfiles (const allProfiles)
  hostEndpoint app getStatistics $ readParams1 "AllStatistics" "cname" 
                                     \c -> withServerConn \conn -> queryAllStatistics conn c
  hostEndpoint app getNewFiles $ readCnameDateParams "GetNewFiles" 
                                   \c d -> withServerConn \conn -> queryNewFiles conn c d
  hostEndpoint app postStatistics \{body} -> withServerConn \c -> addStatistics c body
  hostEndpoint app getProfileFiles $ readParams2 "GetProfileFiles" "cname" "pname" findProfileFiles
  hostFileUploadEndpoint app attachFile $ \(p@{body}) -> readParams2 "AttachFile" "eventid" "name" 
                                                         (\e n -> safeParseIntE "AttachFile" e >>= 
                                                            \eventId -> withServerConn 
                                                              \c -> saveFileToDb c eventId n body) 
                                                         p
  hostFile app "/api/files/:id" $ readParams1 "GetFile" "id" 
                                    \stri -> safeParseIntE "GetFile" stri >>= 
                                      \id -> withServerConn 
                                        \conn -> getFileById conn id
  hostStatic app "static"
  hostStatic app "profiles"
  listen app port
  log $ "Starting server on " ++ show port

------- Query Param helpers ---------------------
readCnameDateParams :: forall a b m. (MonadError Error m) => String -> (String -> Date -> m a) -> 
                                                             Input b -> m a
readCnameDateParams label f i = readParams2 "cname" "date" label 
                               (\cname d -> fromStringStrictE label d >>= \date -> f cname date) i

readParams1 :: forall a m. (MonadError Error m) => String -> String -> (String -> m a) -> 
                                                   Input Unit -> m a
readParams1 label key f {params} =
  maybe (throwError $ error (label <> ": No " <> key <> " provided")) f (lookup key params)

readParams2 :: forall a b m. (MonadError Error m) => 
               String -> String -> String -> (String -> String -> m a) -> 
               Input b -> m a
readParams2 label key1 key2 f {params} = 
  either (\key -> throwError $ error $ label <> ": No" <> key <> " provided") (\(Tuple a b) -> f a b )
    (do e1 <- maybe (Left key1) Right $ lookup key1 params
        e2 <- maybe (Left key2) Right $ lookup key2 params
        return $ Tuple e1 e2)
  
fromStringStrictE :: forall m. (MonadError Error m) => String -> String -> m Date
fromStringStrictE label s = maybe (throwStr $ label <> ": Invalid date provided") return 
                            (fromStringStrict s)

safeParseIntE :: forall m. (MonadError Error m) => String -> String -> m Int
safeParseIntE label s = maybe (throwStr $ label <> ": Invalid int provided") return (safeParseInt s)
 
throwStr :: forall m a. (MonadError Error m) => String -> m a
throwStr = throwError <<< error

------- FileSystem Queries --------------------------
findProfileFiles :: forall eff. String -> String -> Aff (fs :: FS | eff) (Array String)
findProfileFiles cname pname = 
  readdir (concat ["profiles", cname, pname]) >>= 
    \names -> return $ (\path ->  concat [cname, pname, path]) <$> names 
    
allProfiles :: forall eff. Aff (fs :: FS | eff) (Array (Tuple String (Array String)))
allProfiles = do
  let prof = (normalize "./profiles")
  inProfiles <- readdir prof
  dirs <- flip filterM inProfiles \p -> stat (concat [prof, p]) >>= (return <<< isDirectory)
  flip traverse dirs (\dir -> readdir (concat [prof, dir]) >>= (\profs -> return $ Tuple dir profs))

--testFindProfileFiles = runAff (log <<< show) (\s -> traverse log s *> return unit) (findProfileFiles "mycomputername" "profile1")
