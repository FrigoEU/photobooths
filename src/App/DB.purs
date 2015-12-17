module App.DB where
  
import Prelude

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Aff (Aff())

import Data.Date (Date())
import Data.Array (replicate, filter, length)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Maybe
import Data.Traversable
import Data.Monoid (mempty)
import Data.Foreign
import Data.Foreign.Class

import Database.AnyDB (Query(..), execute_, query_, DB(), queryOne_, execute, Connection(), query, queryOne)
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
import App.Model.WorkerState
import App.DB

----- THIS FILE SHOULD NOT HAVE ANYTHING WITH "INPUT" TYPE IN IT ----------

dropDB :: forall eff. Connection -> Aff (db :: DB | eff) Unit
dropDB conn = do
  execute_ (dropTable photoboothsTable) conn
  execute_ (dropTable eventsTable) conn
  execute_ (dropTable savedFileTable) conn
  execute_ (dropTable eventStatisticsTable) conn
  execute_ (dropTable monthlyStatisticsTable) conn
  execute_ (dropTable networkingStateTable) conn
  execute_ (dropTable workerStateTable) conn

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
  execute_ createWorkerStateTable conn
  
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

allPhotobooths :: forall eff. Connection -> Aff (db :: DB | eff) (Array Photobooth)
allPhotobooths conn = query_ (selectStar photoboothsTable "" :: Query Photobooth) conn

queryEvents :: forall eff. Connection -> String -> Aff (db :: DB | eff) (Array Event)
queryEvents conn cname = 
  let q = (Query "Select * from EVENTS WHERE computername = ?" :: Query PartialEvent)
   in do partialEvs <- query q [toSql cname] conn
         let ids = (\(PartialEvent {id: mi}) -> fromMaybe (-1) mi) <$> partialEvs
         files <- if length ids > -1 then selectFilesForEvents ids conn else return []
         let createEvent pe@(PartialEvent {id: mi}) = mkEvent pe (filter (\(SavedFile im) -> im.eventId == (fromMaybe 0 mi)) files)
         return $ createEvent <$> partialEvs


newPB :: forall eff. Connection -> Input Photobooth -> Aff (db :: DB | eff) Photobooth
newPB conn {body: pb} = do
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

newEvent :: forall eff. Connection -> Input Event -> Aff (db :: DB | eff) Event
newEvent conn {body: ev} = do
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

updatePB :: forall eff. Connection -> Input Photobooth -> Aff (db :: DB | eff) Photobooth
updatePB _    ({body: (Photobooth    {id: Nothing})}) =
  throwError $ error $ "updatePhotobooth impossible without id!"
updatePB conn ({body: (Photobooth pb@{id: Just i})}) =
  let query = update photoboothsTable i (fromArray [Tuple "alias" pb.alias, Tuple "defaultprofile" pb.defaultprofile]) ""
   in do execute_ query conn
         res <- queryOne_ (selectStarId photoboothsTable i) conn
         maybe (throwError $ error $ "Failed to update Photobooth") return res

updateEvent :: forall eff. Connection -> Input Event -> Aff (db :: DB | eff) Event
updateEvent _    ({body: (Event    {id: Nothing})}) =
  throwError $ error $ "updateEvent impossible without id"
updateEvent conn ({body: (Event e@{id: Just i}), url: u}) =
  let query = update eventsTable i (fromArray [ Tuple "computername" e.computername
                                              , Tuple "name" e.name
                                              , Tuple "datefrom" $ iso8601 e.datefrom
                                              , Tuple "dateuntil" $ iso8601 e.dateuntil
                                              , Tuple "profile" e.profile
                                              ]) ""
   in do
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

saveFileToDb :: forall eff. Connection -> Int -> String -> Input Buffer 
                            -> Aff (db :: DB | eff) SavedFile
saveFileToDb conn eventId name {body: buff} = do
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
        "Select f.* from FILES f " <>
        "Join Events e on e.id = f.eventid " <>
        "where e.computername = ? " <>
        "and f.updatedon > ? "

addStatistics :: forall eff. Connection -> AllStatistics -> Aff (db :: DB | eff) Unit
addStatistics c (AllStatistics {eventStatistics, monthlyStatistics}) = do
  _ <- traverse (\s -> execute_ (upsertMonthlyStatistic s) c) monthlyStatistics
  _ <- traverse (\s -> execute_ (upsertEventStatistic s) c) eventStatistics
  return unit
  
newtype BufferForHttp = BufferForHttp Buffer

instance foreignBufferForHttp :: IsForeign BufferForHttp where 
  read obj = do
    file <- readProp "file" obj
    return $ BufferForHttp $ unsafeFromForeign file
        
unpack :: BufferForHttp -> Buffer
unpack (BufferForHttp b) = b
  
getFileById :: forall eff. Connection -> Int -> Aff (db :: DB | eff) Buffer
getFileById conn id =
  let q = (Query "select file from files where id = ?" :: Query BufferForHttp)
   in queryOne q [toSql id] conn >>= \mf -> maybe (throwError $ error $ "No file found") (unpack >>> return) mf

updateWorkerState :: forall eff. Connection -> WorkerState -> Aff (db :: DB | eff) Unit 
updateWorkerState c (WorkerState ws) = 
  let q = Query "update WORKERSTATE set activeeventid = ?"
   in execute q [toSql ws.active] c
   
queryActiveEvent :: forall eff. Connection -> Date -> Aff (db :: DB | eff) (Maybe PartialEvent)
queryActiveEvent c d = 
  let q = Query $ "select * from EVENTS where " 
               <> "datefrom < '2015-12-13T10:27:04.038Z' " 
               <> "and dateuntil > '2015-12-13T10:27:04.038Z' "
   in queryOne q [toSql $ iso8601 d] c

getMonthlyStatistic :: forall e. String -> Connection -> Int -> Aff (db :: DB | e) MonthlyStatistic 
getMonthlyStatistic cname c m = 
  let q = Query "select * from monthlystatistics where month = ?"
      handle Nothing = MonthlyStatistic { computername: cname
                                        , month: m
                                        , pictures: 0
                                        , prints: 0}
      handle (Just i) = i
   in queryOne q [toSql m] c >>= (return <<< handle)
