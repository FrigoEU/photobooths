module Main where

import App.DB (makeDB, mainConnectionInfo, getFileById, saveFileToDb, tryLogin, addStatistics, queryNewFiles, queryAllStatistics, queryNewEvents, queryEventsByIds, queryEventsPaged, updateEvent, newEvent, queryEvents, deletePB, updatePB, newPB, allPhotobooths, queryPhotobooth)
import App.Endpoint (attachFile, login, getProfileFiles, postStatistics, getNewFiles, getStatistics, getProfiles, getNewEvents, getEventsByIds, getEventsPaged, putEvents, postEvents, getEvents, deletePhotobooth, putPhotobooths, postPhotobooths, getPhotobooths, getPhotobooth)
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (error, Error, EXCEPTION)
import Control.Monad.Error.Class (throwError, class MonadError)
import DOM.File.Types (Blob)
import Data.Array (filterM)
import Data.Date (Now, now)
import Data.Either (Either(Left))
import Data.Int (ceil)
import Data.Int.Extended (safeParseInt)
import Data.Maybe (Maybe(Just), maybe)
import Data.StrMap (lookup)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Database.AnyDB (DB, Connection, withConnection)
import Node.Buffer (Buffer)
import Node.FS (FS)
import Node.FS.Aff (readdir, stat)
import Node.FS.Stats (isDirectory)
import Node.Path (normalize, concat)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (YargsSetup, example, usage)
import Prelude (const, Unit, return, ($), (>>=), flip, (<<<), bind, (<$>), (<>), show, (++))
import Server.Core (Input, EXPRESS, listen, hostStatic, hostFile, hostFileUploadEndpoint, hostEndpoint, makeApp)
import Unsafe.Coerce (unsafeCoerce)

withServerConn :: forall a eff. (Connection -> Aff (db :: DB | eff) a) -> Aff (db :: DB | eff) a 
withServerConn = withConnection mainConnectionInfo

setup :: YargsSetup
setup = usage "$0 -p port" 
        <> example "$0 -p 8080" "Run photobooth server!"

main :: forall t346. Eff ( err :: EXCEPTION , console :: CONSOLE , now :: Now , db :: DB , express :: EXPRESS , fs :: FS | t346 ) Unit
main = runY setup $ server <$> yarg "p" ["port"] (Just "Port") (Left 8080.0) false

server :: forall eff. Number -> Eff ( now :: Now , console :: CONSOLE , db :: DB , express :: EXPRESS , fs :: FS | eff ) Unit
server p = do
  let port = ceil p
  dateNow <- now
  runAff (log <<< show) (const $ log "Initial SQL OK") $ withConnection mainConnectionInfo \conn -> do
      --dropDB conn
      makeDB conn
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
  hostEndpoint app login \_ {body:(Tuple u p)}-> withServerConn \c -> tryLogin c u p 

  hostFileUploadEndpoint app attachFile \qp {body} -> withServerConn \c -> saveFileToDb c qp body
  hostFile app "/api/files/:id" $ readParams1 "GetFile" "id" 
                                    \stri -> safeParseIntE "GetFile" stri >>= 
                                      \id -> withServerConn 
                                        \conn -> getFileById conn id
  hostStatic app "static"
  hostStatic app "profiles"
  listen app port
  log $ "Starting server on " ++ show port

------- Conversions -----------------------------
blobToBuffer :: Blob -> Buffer
blobToBuffer = unsafeCoerce

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
