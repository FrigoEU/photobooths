module App.Networking where

import Prelude

import Database.AnyDB.SqlValue (toSql)
import Database.AnyDB
import Database.AnyDB.Transaction

import Data.Maybe
import Data.Date
import Data.Tuple
import Data.Traversable
--import Data.Foreign
import Data.Foreign.Class
import DOM.File.Types(Blob())

import Node.Buffer (Buffer())

import Unsafe.Coerce

import Network.HTTP.Affjax 
import Network.HTTP.Method (Method(..))

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class
import Control.Monad.Aff (runAff)
import Control.Monad.Error.Class (throwError, MonadError)
import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Aff (Aff())
import Control.Alt
import Control.Bind (join)
import Control.Apply((*>))

import App.DB
import App.Endpoint

networkingConnectionInfo :: ConnectionInfo
networkingConnectionInfo = Sqlite3
  { filename: "networkingdb"
  , memory: false }
  
main = do
  let cname = "mycomputername"
  let baseurl = "http://localhost:8080"
  runAff (log <<< show) (const $ log "Everything synced!") $ withConnection networkingConnectionInfo $ \conn -> do
    --dropDB conn
    --makeDB conn
    liftEff $ log "Initial SQL Done"
    pbm <- execEndpoint_ baseurl getPhotobooth cname unit
    case pbm of
         Nothing -> throwError $ error $ "No photobooth instance found for: " <> cname
         Just pb -> execute_ (upsertPB pb) conn
    liftEff $ log "Synced Photobooth(s)"
    maybeLastUpdatedEvents <- queryOne getLastUpdateEvents [] conn
    lastUpdatedEvents <- defaultDate maybeLastUpdatedEvents (fromStringStrict "2010-01-01T00:00:00.000Z")
    pes <- execEndpoint_ baseurl getNewEvents (Tuple cname lastUpdatedEvents) unit
    withTransaction (\tc -> traverse (\pe -> execute_ (upsertPartialEvent pe) conn) pes) conn
    liftEff $ log "Synced Events"
    maybeLastUpdatedFiles <- queryOne getLastUpdateFiles [] conn
    lastUpdatedFiles <- defaultDate maybeLastUpdatedFiles (fromStringStrict "2010-01-01T00:00:00.000Z")
    files <- execEndpoint_ baseurl getNewFiles (Tuple cname lastUpdatedFiles) unit
    withTransaction (\tc -> traverse (\sf -> execute_ (upsertSavedFile sf) conn) files) conn
    liftEff $ log "Synced Files"
    eventids <- query getEmptyFileEntries [] conn
    flip traverse eventids (\(WrappedId id) -> downloadFileById baseurl id >>= (\b -> saveFile conn id b))
    stats <- queryAllStatistics conn cname
    execEndpoint_ baseurl postStatistics unit stats
    liftEff $ log "Synced statistics"
    liftEff $ log "Synced File contents"
    
    
data LastUpdated = LastUpdated String
               
instance isForeignLastUpdated :: IsForeign LastUpdated where
  read obj = do
    lastupdatedon <- readProp "lastupdatedon" obj
    return $ LastUpdated lastupdatedon
    
toStr :: LastUpdated -> String
toStr (LastUpdated s) = s

data WrappedId = WrappedId Int
instance isForeignWrappedId :: IsForeign WrappedId where
  read obj = do
    id <- readProp "id" obj
    return $ WrappedId id
    
getLastUpdateEvents :: Query LastUpdated
getLastUpdateEvents = Query $ "select coalesce(max(updatedon), '2010-01-01 00:00:00') as lastupdatedon from events"
                                     
getLastUpdateFiles :: Query LastUpdated
getLastUpdateFiles = Query $ "select coalesce(max(updatedon), '2010-01-01 00:00:00') as lastupdatedon from files"
                                     
getEmptyFileEntries :: Query WrappedId
getEmptyFileEntries = Query $ "select id from files where file is NULL"
                                                          
downloadFileById :: forall eff. String -> Int -> Aff (ajax :: AJAX | eff) Buffer
downloadFileById s i = do
  let req = {headers: [], content: Nothing, method: GET, url: s <> "/api/files/" <> show i, username: Nothing, password: Nothing } :: AffjaxRequest Unit 
  res :: AffjaxResponse Buffer <- affjax req 
  case res of 
       {response} -> return response

saveFile :: forall eff. Connection -> Int -> Buffer -> Aff (db :: DB | eff) Unit
saveFile c i b = execute (Query "update files set file = ? where id = ?") [toSql b, toSql i] c
                                     
defaultDate :: forall m. (MonadError Error m) => Maybe LastUpdated -> Maybe Date -> m Date
defaultDate ms md = maybe (throwError $ error "Failed to parse date - last updated events") return ((join $ fromString <$> (toStr <$> ms)) <|> md)
