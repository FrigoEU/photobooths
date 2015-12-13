module App.DB where
  
import Prelude

import Control.Monad.Eff.Exception (error)
--import Control.Monad.Eff.Class
import Control.Monad.Aff (Aff())
import Control.Monad.Error.Class (throwError)
--import Control.Monad.Eff.Console (log, CONSOLE())

import Data.Date (Date())
import Data.Array (replicate, filter, length)
import Data.StrMap (lookup)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Maybe
import Data.Traversable
import Data.Monoid (mempty)
import Data.Foreign
import Data.Foreign.Class
--import Control.Apply ((*>))

import Database.AnyDB (ConnectionInfo (..), Query(..), withConnection, execute_, query_, DB(), queryOne_, execute, Connection(), query, queryOne)
import Database.AnyDB.SqlValue (toSql)
import Node.Buffer (Buffer())

import SQL
import Server.Core

import App.Model.Photobooth
import App.Model.Event
import App.Model.SavedFile
import App.Model.Statistic
import App.Model.Date
import App.Model.StrMap
import App.Model.NetworkingState
import App.DB

----- THIS FILE SHOULD NOT HAVE ANYTHING WITH "INPUT" TYPE IN IT ----------

connectionInfo :: ConnectionInfo
connectionInfo = Sqlite3
  { filename: "klikhutdb"
  , memory: false }

dropDB :: forall eff. Connection -> Aff (db :: DB | eff) Unit
dropDB conn = do
  execute_ (dropTable photoboothsTable) conn
  execute_ (dropTable eventsTable) conn
  execute_ (dropTable savedFileTable) conn
  execute_ (dropTable eventStatisticsTable) conn
  execute_ (dropTable monthlyStatisticsTable) conn
  execute_ (dropTable networkingStateTable) conn

makeDB :: forall eff. Connection -> Aff (db :: DB | eff) Unit
makeDB conn = do
  execute_ (createTable photoboothsTable) conn
  execute_ (updatedonInsertTrigger photoboothsTable ["id"]) conn
  execute_ (updatedonUpdateTrigger photoboothsTable ["id"]) conn
  execute_ (createTable eventsTable) conn
  execute_ (updatedonInsertTrigger eventsTable ["id"]) conn
  execute_ (updatedonUpdateTrigger eventsTable ["id"]) conn
  execute_ (createTable savedFileTable) conn
  execute_ (updatedonInsertTrigger savedFileTable ["id"]) conn
  execute_ (updatedonUpdateTrigger savedFileTable ["id"]) conn
  execute_ (Query createEventStatisticsTable) conn
  execute_ (updatedonInsertTrigger eventStatisticsTable ["computername", "eventid"]) conn
  execute_ (updatedonUpdateTrigger eventStatisticsTable ["computername", "eventid"]) conn
  execute_ (Query createMonthlyStatisticsTable) conn
  execute_ (updatedonInsertTrigger monthlyStatisticsTable ["computername", "month"]) conn
  execute_ (updatedonUpdateTrigger monthlyStatisticsTable ["computername", "month"]) conn
  execute_ createNetworkingStateTable conn
  
loadWithDummy :: forall eff. Connection -> Date -> Aff (db :: DB | eff) Unit
loadWithDummy conn dateNow = do
  execute_ (insertPB mybooth) conn
  execute_ (insertPB yourbooth) conn
  execute_ (insertEvent (myevent dateNow)) conn
  execute_ (upsertEventStatistic myeventstatistic) conn
  execute_ (upsertMonthlyStatistic mymonthlystatistic) conn
  
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
                  , files: []}

myeventstatistic :: EventStatistic
myeventstatistic = EventStatistic { eventId: 1
                                  , computername: "mycomputername"
                                  , pictures: 200
                                  , prints: 190 }

mymonthlystatistic :: MonthlyStatistic
mymonthlystatistic = MonthlyStatistic { month: 1
                                      , computername: "mycomputername"
                                      , pictures: 500
                                      , prints: 390 }

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
              let ids = (\(PartialEvent {id: mi}) -> fromMaybe 0 mi) <$> partialEvs
              files <- if length ids > 0 then selectFilesForEvents ids conn else return []
              let createEvent pe@(PartialEvent {id: mi}) = mkEvent pe (filter (\(SavedFile im) -> im.eventId == (fromMaybe 0 mi)) files)
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
                                         ]) false ""
                                         
upsertPB :: Photobooth -> Query Unit
upsertPB (Photobooth pb) = insert photoboothsTable
                              (fromArray [ maybe mempty (\s -> Tuple "id" $ show s) pb.id
                                         , Tuple "computername" pb.computername
                                         , Tuple "alias" pb.alias
                                         , Tuple "defaultprofile" pb.defaultprofile
                                         ]) true ""

newEvent :: forall eff. Input Event -> Aff (db :: DB | eff) Event
newEvent ({body: ev}) =
  withConnection connectionInfo \conn -> do
    execute_ (insertEvent ev) conn
    res <- queryOne_ (selectLastInserted eventsTable) conn
    case res of
         Nothing -> throwError $ error $ "Failed to create Event"
         Just pe@(PartialEvent {id: Nothing}) -> return $ mkEvent pe []
         Just pe@(PartialEvent {id: Just i}) -> selectFilesForEvent i conn >>= \ims -> return $ mkEvent pe ims

insertEvent :: Event -> Query Unit
insertEvent (Event e) = insert eventsTable
                        (fromArray [ Tuple "computername" e.computername
                                   , Tuple "name" e.name
                                   , Tuple "datefrom" $ iso8601 e.datefrom
                                   , Tuple "dateuntil" $ iso8601 e.dateuntil
                                   , Tuple "profile" $ e.profile
                                   ]) false ""
                                   
upsertPartialEvent :: PartialEvent -> Query Unit
upsertPartialEvent (PartialEvent e) = insert eventsTable
                        (fromArray [ maybe mempty (\s -> Tuple "id" $ show s) e.id
                                   , Tuple "computername" e.computername
                                   , Tuple "name" e.name
                                   , Tuple "datefrom" $ iso8601 e.datefrom
                                   , Tuple "dateuntil" $ iso8601 e.dateuntil
                                   , Tuple "profile" $ e.profile
                                   ]) true ""

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
           Just pe@(PartialEvent {id: Just i}) -> selectFilesForEvent i conn >>= \ims -> return $ mkEvent pe ims

upsertSavedFile :: SavedFile -> Query Unit
upsertSavedFile (SavedFile sf) = insert savedFileTable
                              (fromArray [ Tuple "id" $ show sf.id
                                         , Tuple "name" sf.name
                                         , Tuple "eventId" $ show sf.eventId
                                         ]) true ""

saveFileToDb :: forall eff. Input Buffer -> Aff (db :: DB | eff) SavedFile
saveFileToDb ({body: buff, params: params}) = do
  case lookup "eventid" params of
       Nothing -> throwError $ error "SaveFileToDb: No eventid provided"
       Just str -> case safeParseInt str of
                        Nothing -> throwError $ error $ "SaveFileToDb: Failed to parse: " <> str
                        Just eventId -> case lookup "name" params of
                                             Nothing -> throwError $ error "SaveFileToDb: No name provided"
                                             Just name -> withConnection connectionInfo \conn -> do
                                               let query = Query $ "INSERT INTO " <> savedFileTable.name <> " (name, eventid, File) VALUES (?, ?, ?)"
                                               execute query [toSql name, toSql eventId, toSql buff] conn
                                               res <- queryOne_ (selectLastInserted savedFileTable) conn
                                               maybe (throwError $ error $ "Failed to save File") return res

selectFilesForEvent :: forall eff. Int -> Connection -> Aff (db :: DB | eff) (Array SavedFile)
selectFilesForEvent i conn = query (selectStar savedFileTable "WHERE eventid = ?") [toSql i] conn

selectFilesForEvents :: forall eff. Array Int -> Connection -> Aff (db :: DB | eff) (Array SavedFile)
selectFilesForEvents is conn =
  query (selectStar savedFileTable $ "WHERE eventid = (" <> qs <>")") (map toSql is) conn
    where
      qs = joinWith ", " (replicate (length is) "?")

upsertEventStatistic :: EventStatistic -> Query Unit
upsertEventStatistic (EventStatistic e) = insert eventStatisticsTable
                                 (fromArray [ Tuple "eventId" $ show e.eventId
                                            , Tuple "computername" $ e.computername
                                            , Tuple "pictures" $ show e.pictures
                                            , Tuple "prints" $ show e.prints
                                            ]) true ""

upsertMonthlyStatistic :: MonthlyStatistic -> Query Unit
upsertMonthlyStatistic (MonthlyStatistic e) = insert monthlyStatisticsTable
                                   (fromArray [ Tuple "month" $ show e.month
                                              , Tuple "computername" $ e.computername
                                              , Tuple "pictures" $ show e.pictures
                                              , Tuple "prints" $ show e.prints
                                              ]) true ""

allStatistics :: forall eff. Input Unit -> Aff (db :: DB | eff) AllStatistics
allStatistics {params: params} =
  case lookup "cname" params of
       Nothing -> throwError $ error "GetStatistics: No computername provided"
       Just cname -> withConnection connectionInfo \conn -> queryAllStatistics conn cname
       
queryAllStatistics :: forall eff. Connection -> String -> Aff (db :: DB | eff) AllStatistics
queryAllStatistics conn cname = do
  eventStatistics <- query (Query "select * from eventstatistics where computername = ?") [toSql cname] conn
  monthlyStatistics <- query (Query "select * from monthlystatistics where computername = ?") [toSql cname] conn
  return $ AllStatistics {eventStatistics, monthlyStatistics}

foreign import pInt :: forall a. Maybe a -> (a -> Maybe a) -> String -> Maybe Int

safeParseInt :: String -> Maybe Int
safeParseInt str = pInt Nothing Just str

queryNewEvents :: forall eff. Connection -> String -> Date -> Aff (db :: DB | eff) (Array PartialEvent)
queryNewEvents conn cname d = query q [toSql cname, toSql d] conn
  where
    q = Query $
        "Select * from EVENTS " <>
        "where computername = ? " <>
        "and updatedon > ?"

queryPhotobooth :: forall eff. Connection -> String -> Aff (db :: DB | eff) (Maybe Photobooth)
queryPhotobooth conn cname = queryOne q [toSql cname] conn
  where
    q = Query $
        "Select * from PHOTOBOOTHS " <>
        "where computername = ?"

queryNewFiles :: forall eff. Connection -> String -> Date -> Aff (db :: DB | eff) (Array SavedFile)
queryNewFiles conn cname d = query q [toSql cname, toSql d] conn
  where
    q = Query $
        "Select * from FILES i " <>
        "Join Events e on e.id = i.eventid " <>
        "where e.computername = ? " <>
        "and i.updatedon > ? "

addStatistics :: forall eff. Connection -> AllStatistics -> Aff (db :: DB | eff) Unit
addStatistics c (AllStatistics {eventStatistics, monthlyStatistics}) = do
  _ <- traverse (\s -> execute_ (upsertMonthlyStatistic s) c) monthlyStatistics
  _ <- traverse (\s -> execute_ (upsertEventStatistic s) c) eventStatistics
  return unit
  
newtype BufferForHttp = BufferForHttp Buffer

instance foreignBufferForHttp :: IsForeign BufferForHttp where 
  read obj = do
<<<<<<< HEAD
    file :: Foreign <- readProp "file" obj
=======
    file <- readProp "file" obj
>>>>>>> aa9c8a3238986319835a130ce823cb8efd6e7206
    return $ BufferForHttp $ unsafeFromForeign file
        
unpack :: BufferForHttp -> Buffer
unpack (BufferForHttp b) = b
  
getFileById :: forall eff. Input Unit -> Aff (db :: DB | eff) Buffer
getFileById {params} = do
  let q = (Query "select file from files where id = ?" :: Query BufferForHttp)
  case lookup "id" params of
         Nothing -> throwError $ error $ "getFileById: No id provided!"
         Just id -> withConnection connectionInfo (\c -> 
           queryOne q [toSql id] c >>= \mf -> maybe (throwError $ error $ "No file found") (unpack >>> return) mf)

