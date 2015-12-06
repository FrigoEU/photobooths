module App.Networking where

import Prelude

import Database.AnyDB.SqlValue (toSql)
import Database.AnyDB
import Database.AnyDB.Transaction

import Data.Maybe
import Data.Date
import Data.Tuple
import Data.Traversable
import Data.Foreign

import Node.Buffer (Buffer())

import Unsafe.Coerce

import Network.HTTP.Affjax 
import Network.HTTP.Method (Method(..))

import Control.Monad.Eff.Console (log)
import Control.Monad.Aff (runAff)
import Control.Monad.Error.Class (throwError, MonadError)
import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Aff (Aff())
import Control.Alt
import Control.Bind (join)

import App.DB
import App.Endpoint

networkingConnectionInfo :: ConnectionInfo
networkingConnectionInfo = Sqlite3
  { filename: "networkingdb"
  , memory: false }
  
main = do
  let cname = "mycomputername"
  let baseurl = "localhost:8080/"
  runAff (log <<< show) (const $ log "Setup SQL OK") $ withConnection networkingConnectionInfo $ \conn -> do
    dropDB conn
    makeDB conn
  runAff (log <<< show) (const $ log "Synced PB") $ withConnection networkingConnectionInfo $ \conn -> do 
    pbm <- execEndpoint_ baseurl getPhotobooth cname unit
    case pbm of
         Nothing -> throwError $ error $ "No photobooth instance found for: " <> cname
         Just pb -> execute_ (upsertPB pb) conn
  runAff (log <<< show) (const $ log "Synced Events") $ withConnection networkingConnectionInfo $ \conn -> do
    maybeLastUpdatedEvents <- queryOne getLastUpdateEvents [toSql cname] conn
    lastUpdatedEvents <- defaultDate maybeLastUpdatedEvents (fromStringStrict "2010-01-01T00:00:00.000Z")
    pes <- execEndpoint_ baseurl getNewEvents (Tuple cname lastUpdatedEvents) unit
    withTransaction (\tc -> traverse (\pe -> execute_ (upsertPartialEvent pe) conn) pes) conn
  runAff (log <<< show) (const $ log "Synced Files") $ withConnection networkingConnectionInfo $ \conn -> do
    maybeLastUpdatedFiles <- queryOne getLastUpdateFiles [toSql cname] conn
    lastUpdatedFiles <- defaultDate maybeLastUpdatedFiles (fromStringStrict "2010-01-01T00:00:00.000Z")
    files <- execEndpoint_ baseurl getNewFiles (Tuple cname lastUpdatedFiles) unit
    withTransaction (\tc -> traverse (\sf -> execute_ (upsertSavedFile sf) conn) files) conn
  runAff (log <<< show) (const $ log "Synced File contents") $ withConnection networkingConnectionInfo $ \conn -> do
    eventids <- query getEmptyFileEntries [] conn
    flip traverse eventids (\id -> downloadFileById baseurl id >>= (\b -> saveFile conn id b))
    
    
getLastUpdateEvents :: Query String
getLastUpdateEvents = Query $ "select updatedon from events "<>
                              "where computername = ?"
                                     
getLastUpdateFiles :: Query String
getLastUpdateFiles = Query $ "select updatedon from files "<>
                              "where computername = ?"
                                     
getEmptyFileEntries :: Query Int
getEmptyFileEntries = Query $ "select id from files where file is NULL"
                                                          
downloadFileById :: forall eff. String -> Int -> Aff (ajax :: AJAX | eff) Buffer
downloadFileById s i = do
  let req = {headers: [], content: Nothing, method: GET, url: s <> "files/" <> show i, username: Nothing, password: Nothing } :: AffjaxRequest Unit 
  res :: AffjaxResponse Foreign <- affjax req 
  case res of 
       {response} -> return $ foreignToBuffer response

foreignToBuffer :: Foreign -> Buffer
foreignToBuffer = unsafeCoerce

saveFile :: forall eff. Connection -> Int -> Buffer -> Aff (db :: DB | eff) Unit
saveFile c i b = execute (Query "update files set file = ? where id = ?") [toSql i, toSql b] c
                                     
defaultDate :: forall m. (MonadError Error m) => Maybe String -> Maybe Date -> m Date
defaultDate ms md = maybe (throwError $ error "Failed to parse date - last updated events") return ((join $ fromString <$> ms) <|> md)
