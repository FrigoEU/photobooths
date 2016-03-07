module Main where

import Prelude (Unit, return, ($), (>>=), flip, (<<<), bind, (<$>), (<>), show, (++))

import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (Aff())
import Control.Monad.Error.Class (throwError, class MonadError)

import Data.Date (Now, now)
import Data.Array (filterM)
import Data.StrMap (lookup)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe)
import Data.Traversable (traverse) 

import Database.AnyDB (DB, Connection, withConnection)
import Node.Path (normalize, concat)
import Node.FS (FS())
import Node.FS.Aff (readdir, stat)
import Node.FS.Stats (isDirectory)

import Server.Core 

import App.Endpoint
import App.DB

port :: Int
port = 8080

withServerConn :: forall a eff. (Connection -> Aff (db :: DB | eff) a) -> Aff (db :: DB | eff) a 
withServerConn = withConnection mainConnectionInfo

main :: forall eff. Eff (now :: Now, console :: CONSOLE, db :: DB, express :: EXPRESS, fs :: FS | eff) Unit
main = do
  dateNow <- now
  --runAff (log <<< show) (const $ log "Initial SQL OK") $ withConnection mainConnectionInfo \conn -> do
      --dropDB conn
      --makeDB conn
      --loadWithDummy conn dateNow
  app <- makeApp
  hostEndpoint app getPhotobooth   \ s _        -> withServerConn \conn -> queryPhotobooth conn s
  hostEndpoint app getPhotobooths  \ _ _        -> withServerConn allPhotobooths
  hostEndpoint app postPhotobooths \ _ {body}   -> withServerConn \conn -> newPB conn body
  hostEndpoint app putPhotobooths  \ _ {body}   -> withServerConn \conn -> updatePB conn body
  hostEndpoint app deletePhotobooth \s _        -> withServerConn \conn -> deletePB conn s
  hostEndpoint app getEvents       \ s _        -> withServerConn \conn -> queryEvents conn s
  hostEndpoint app postEvents      \ _ {body}   -> withServerConn \conn -> newEvent conn body
  hostEndpoint app putEvents       \ _ {body}   -> withServerConn \conn -> updateEvent conn body
  hostEndpoint app getEventsPaged  \(Tuple s i)_-> withServerConn \conn-> queryEventsPaged conn i s
  hostEndpoint app getEventsByIds \ ids _       -> withServerConn \conn-> queryEventsByIds conn ids

  hostEndpoint app getNewEvents    \ qp _     -> withServerConn \conn -> queryNewEvents conn qp
  hostEndpoint app getProfiles     \ _ _      -> allProfiles
  hostEndpoint app getStatistics   \ s _      -> withServerConn \conn -> queryAllStatistics conn s
  hostEndpoint app getNewFiles     \ qp _     -> withServerConn \conn -> queryNewFiles conn qp
  hostEndpoint app postStatistics  \ _ {body} -> withServerConn \c -> addStatistics c body
  hostEndpoint app getProfileFiles \ qp _     -> findProfileFiles qp
  hostFileUploadEndpoint app attachFile \qp {body} -> withServerConn \c -> saveFileToDb c qp body
  hostFile app "/api/files/:id" $ readParams1 "GetFile" "id" 
                                    \stri -> safeParseIntE "GetFile" stri >>= 
                                      \id -> withServerConn 
                                        \conn -> getFileById conn id
  hostStatic app "static"
  hostStatic app "profiles"
  listen app port
  log $ "Starting server on " ++ show port

------- Query Param helpers ---------------------
readParams1 :: forall a m. (MonadError Error m) => String -> String -> (String -> m a) -> 
                                                   Input Unit -> m a
readParams1 label key f {params} =
  maybe (throwError $ error (label <> ": No " <> key <> " provided")) f (lookup key params)

safeParseIntE :: forall m. (MonadError Error m) => String -> String -> m Int
safeParseIntE label s = maybe (throwStr $ label <> ": Invalid int provided") return (safeParseInt s)
 
throwStr :: forall m a. (MonadError Error m) => String -> m a
throwStr = throwError <<< error

------- FileSystem Queries --------------------------
findProfileFiles :: forall eff. Tuple String String -> Aff (fs :: FS | eff) (Array String)
findProfileFiles (Tuple cname pname) = 
  readdir (concat ["profiles", cname, pname]) >>= 
    \names -> return $ (\path ->  concat [cname, pname, path]) <$> names 
    
allProfiles :: forall eff. Aff (fs :: FS | eff) (Array (Tuple String (Array String)))
allProfiles = do
  let prof = (normalize "./profiles")
  inProfiles <- readdir prof
  dirs <- flip filterM inProfiles \p -> stat (concat [prof, p]) >>= (return <<< isDirectory)
  flip traverse dirs (\dir -> readdir (concat [prof, dir]) >>= (\profs -> return $ Tuple dir profs))

--testFindProfileFiles = runAff (log <<< show) (\s -> traverse log s *> return unit) (findProfileFiles "mycomputername" "profile1")
