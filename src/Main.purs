module Main where

import Prelude
 
import Control.Monad.Eff.Exception (Error(), message, error)
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (runAff, Aff())
import Control.Monad.Error.Class (throwError)

import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Encode (encodeJson, EncodeJson)
import Data.Date (now, Now(), Date())
import Data.Array (tail, replicate, filter, length)
import Data.StrMap (lookup)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Maybe

import Database.AnyDB (ConnectionInfo (..), Query(..), withConnection, execute_, query_, DB(), queryOne_, execute, Connection(), query)
import Database.AnyDB.SqlValue (toSql)
import Node.Buffer (Buffer())

import SQL
import Server.Core

import App.Endpoint
import App.Debug
import App.Model.Photobooth
import App.Model.Event
import App.Model.SavedImage
import App.Model.Date
import App.Model.StrMap

connectionInfo :: ConnectionInfo
connectionInfo = Sqlite3  
  { filename: "klikhutdb"
  , memory: false }

mybooth :: Photobooth
mybooth = Photobooth { id: Nothing
                     , computername: "mycomputername"
                     , alias: "myalias"
                     , defaultprofile: "myprofile" }
yourbooth :: Photobooth
yourbooth = Photobooth { id: Nothing
                       , computername: "yourcomputername"
                       , alias: "youralias"
                       , defaultprofile: "yourprofile" }

myevent :: Date -> Event
myevent d = Event { id: Nothing
                  , computername: "mycomputername"
                  , name: "Sweet Event Dude"
                  , datefrom: d
                  , dateuntil: d
                  , profile: "myprofile" 
                  , images: []}

port :: Int
port = 8080

main = do
  dateNow <- now
  runAff (log <<< show) (const $ log "Initial SQL OK") $ do
    withConnection connectionInfo \conn -> do
      execute_ (dropTable photoboothsTable) conn
      execute_ (createTable photoboothsTable) conn
      execute_ (insertPB mybooth) conn
      execute_ (insertPB yourbooth) conn
      execute_ (dropTable eventsTable) conn
      execute_ (createTable eventsTable) conn
      execute_ (insertEvent (myevent dateNow)) conn
      execute_ (dropTable savedImageTable) conn
      execute_ (createTable savedImageTable) conn
  app <- makeApp
  hostEndpoint app getPhotobooths allPhotobooths
  hostEndpoint app postPhotobooths newPB
  hostEndpoint app putPhotobooths updatePB
  hostEndpoint app getEvents eventsByCname
  hostEndpoint app postEvents newEvent
  hostEndpoint app putEvents updateEvent
  hostFileEndpoint app attachImage saveImageToDb
  hostFile app "*" static
  listen app port 
  log $ "Starting server on " ++ show port 

static :: forall eff. Input Unit -> Aff (console :: CONSOLE | eff) String
static {url: "/"} = return "./static/index.html"
static {url: "/static/client.js"} = return "./static/client.js"
static {url: a} = return a

allPhotobooths :: forall eff. Input Unit -> Aff (db :: DB | eff) (Array Photobooth)
allPhotobooths _ = withConnection connectionInfo 
                   \conn -> query_ (selectStar photoboothsTable "" :: Query Photobooth) conn

eventsByCname :: forall eff. Input Unit -> Aff (db :: DB | eff) (Array Event)
eventsByCname {params: params} = 
  case lookup "cname" params of
       Nothing -> throwError $ error "GetEvents: No computername provided"
       Just cname -> 
         let q = selectStar eventsTable (" WHERE computername = '" <> cname <> "'") :: Query PartialEvent
          in withConnection connectionInfo \conn -> do 
              partialEvs <- query_ q conn
              let ids = (\(PartialEvent {id: Just i}) -> i) <$> partialEvs
              images <- selectImagesForEvents ids conn
              let createEvent pe@(PartialEvent {id: Just i}) = mkEvent pe (filter (\(SavedImage im) -> im.eventId == i) images)
              return $ createEvent <$> partialEvs


newPB :: forall eff. Input Photobooth -> Aff (db :: DB | eff) Photobooth
newPB ({body: pb}) = withConnection connectionInfo \conn -> do 
  execute_ (insertPB pb) conn
  res <- queryOne_ (selectLastInserted photoboothsTable) conn
  maybe (throwError $ error $ "Failed to create Photobooth") return res

insertPB :: Photobooth -> Query Unit
insertPB (Photobooth pb) = insert photoboothsTable 
                              (fromArray [ Tuple "computername" pb.computername
                                         , Tuple "alias" pb.alias
                                         , Tuple "defaultprofile" pb.defaultprofile
                                         ]) ""

newEvent :: forall eff. Input Event -> Aff (db :: DB | eff) Event
newEvent ({body: ev}) = 
  withConnection connectionInfo \conn -> do
    execute_ (insertEvent ev) conn
    res <- queryOne_ (selectLastInserted eventsTable) conn
    case res of
         Nothing -> throwError $ error $ "Failed to create Event"
         Just pe@(PartialEvent {id: Nothing}) -> return $ mkEvent pe []
         Just pe@(PartialEvent {id: Just i}) -> selectImagesForEvent i conn >>= \ims -> return $ mkEvent pe ims

insertEvent :: Event -> Query Unit
insertEvent (Event e) = insert eventsTable 
                        (fromArray [ Tuple "computername" e.computername
                                   , Tuple "name" e.name
                                   , Tuple "datefrom" $ iso8601 e.datefrom
                                   , Tuple "dateuntil" $ iso8601 e.dateuntil
                                   , Tuple "profile" $ e.profile
                                   ]) ""

updatePB :: forall eff. Input Photobooth -> Aff (db :: DB | eff) Photobooth
updatePB ({body: (Photobooth    {id: Nothing})}) = 
  throwError $ error $ "updatePhotobooth impossible without id!"
updatePB ({body: (Photobooth pb@{id: Just i})}) =
  let query = update photoboothsTable i (fromArray [Tuple "alias" pb.alias, Tuple "defaultprofile" pb.defaultprofile]) ""
   in withConnection connectionInfo \conn -> do
      execute_ query conn
      res <- queryOne_ (selectStarId photoboothsTable i) conn
      maybe (throwError $ error $ "Failed to update Photobooth") return res

updateEvent :: forall eff. Input Event -> Aff (db :: DB | eff) Event
updateEvent ({body: (Event    {id: Nothing})}) = 
  throwError $ error $ "updateEvent impossible without id"
updateEvent ({body: (Event e@{id: Just i}), params: p, url: u}) = 
  let query = update eventsTable i (fromArray [ Tuple "computername" e.computername 
                                              , Tuple "name" e.name
                                              , Tuple "datefrom" $ iso8601 e.datefrom
                                              , Tuple "dateuntil" $ iso8601 e.dateuntil
                                              , Tuple "profile" e.profile
                                              ]) ""
   in withConnection connectionInfo \conn -> do
      execute_ query conn
      res <- queryOne_ (selectStarId eventsTable i) conn
      case res of
           Nothing -> throwError $ error $ "Failed to update Event"
           Just pe@(PartialEvent {id: Nothing}) -> return $ mkEvent pe []
           Just pe@(PartialEvent {id: Just i}) -> selectImagesForEvent i conn >>= \ims -> return $ mkEvent pe ims

saveImageToDb :: forall eff. Input Buffer -> Aff (db :: DB | eff) SavedImage
saveImageToDb ({body: buff, params: params}) = do
  case lookup "eventid" params of
       Nothing -> throwError $ error "SaveImageToDb: No eventid provided"
       Just str -> case safeParseInt str of
                        Nothing -> throwError $ error $ "SaveImageToDb: Failed to parse: " <> str
                        Just eventId -> case lookup "name" params of 
                                             Nothing -> throwError $ error "SaveImageToDb: No name provided"
                                             Just name -> withConnection connectionInfo \conn -> do
                                               let query = Query $ "INSERT INTO " <> savedImageTable.name <> " (name, eventid, image) VALUES (?, ?, ?)"
                                               execute query [toSql name, toSql eventId, toSql buff] conn
                                               res <- queryOne_ (selectLastInserted savedImageTable) conn
                                               maybe (throwError $ error $ "Failed to save image") return res

selectImagesForEvent :: forall eff. Int -> Connection -> Aff (db :: DB | eff) (Array SavedImage)
selectImagesForEvent i conn = query (selectStar savedImageTable "WHERE eventid = ?") [toSql i] conn

selectImagesForEvents :: forall eff. Array Int -> Connection -> Aff (db :: DB | eff) (Array SavedImage)
selectImagesForEvents is conn = 
  query (selectStar savedImageTable $ "WHERE eventid = (" <> qs <>")") (map toSql is) conn
    where 
      qs = joinWith ", " (replicate (length is) "?")

foreign import pInt :: forall a. Maybe a -> (a -> Maybe a) -> String -> Maybe Int

safeParseInt :: String -> Maybe Int
safeParseInt str = pInt Nothing Just str
