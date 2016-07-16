module App.DB where
  
import App.DB (MyFile(MyFile), unpack, upsertEventStatistic, upsertMonthlyStatistic, selectFilesForEvent, insertEvent, insertPB, selectFilesForEvents, addFilesToEvents, myuser, newUser, mymonthlystatistic, myeventstatistic, myevent, yourbooth, mybooth)
import App.Model.Date (toISOString, dateToSqlForComparing)
import App.Model.Event (Event(Event), PartialEvent(PartialEvent), mkEvent, eventsTable)
import App.Model.NetworkingState (insertDefaultNetworkingSate, createNetworkingStateTable, networkingStateTable)
import App.Model.Photobooth (Photobooth(Photobooth), photoboothsTable)
import App.Model.SavedFile (SavedFile(SavedFile), savedFileTable)
import App.Model.Session (Session, sessionsTable)
import App.Model.Statistic (AllStatistics(AllStatistics), EventStatistic(EventStatistic), MonthlyStatistic(MonthlyStatistic), monthlyStatisticsTable, eventStatisticsTable, createMonthlyStatisticsTable, createEventStatisticsTable)
import App.Model.StrMap (fromArray)
import App.Model.User (User(User), usersTable)
import App.Model.WorkerState (WorkerState(WorkerState), createWorkerStateTable, workerStateTable)
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Array (replicate, filter, length)
import Data.Date (Now, Date, now)
import Data.Foreign (unsafeFromForeign)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe)
import Data.Monoid (mempty)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Database.AnyDB (DB, Connection, ConnectionInfo(Sqlite3), Query(Query), queryOne_, execute, queryOne, execute_, query, query_)
import Database.AnyDB.SqlValue (toSql)
import Node.Buffer (Buffer)
import Prelude (Unit, return, ($), bind, (<<<), (>>=), (<>), (>>>), unit, show, map, (<$>), (==), negate, (>), (*))
import SQL (selectLastInserted, insert, selectStar, selectStarId, update, createTable, updatedonUpdateTrigger, updatedonInsertTrigger, dropTable)
import Unsafe.Coerce (unsafeCoerce)

networkingConnectionInfo :: ConnectionInfo
networkingConnectionInfo = Sqlite3
  { filename: "networkingdb"
  , memory: false }
  
mainConnectionInfo :: ConnectionInfo
mainConnectionInfo = Sqlite3
  { filename: "klikhutdb"
  , memory: false }

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
  execute_ (dropTable usersTable) conn
  execute_ (dropTable sessionsTable) conn

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
  execute_ insertDefaultNetworkingSate conn
  execute_ createWorkerStateTable conn
  execute_ (createTable usersTable) conn
  execute_ (createTable sessionsTable) conn
  
loadWithDummy :: forall eff. Connection -> Date -> Aff (db :: DB | eff) Unit
loadWithDummy conn dateNow = do
  {-- execute_ (insertPB mybooth) conn --}
  {-- execute_ (insertPB yourbooth) conn --}
  {-- execute_ (insertEvent (myevent dateNow)) conn --}
  {-- execute_ (upsertEventStatistic myeventstatistic) conn --}
  {-- execute_ (upsertMonthlyStatistic mymonthlystatistic) conn --}
  execute_ (newUser myuser) conn
  
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

myuser :: User
myuser = User { id: 1
              , name: "admin"
              , password: "test"}

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
  let q = Query "Select * from EVENTS WHERE computername = ?" :: Query PartialEvent
   in query q [toSql cname] conn >>= addFilesToEvents conn

queryEventsPaged :: forall eff. Connection -> Int -> String -> Aff (db :: DB | eff) (Array Event)
queryEventsPaged conn i cname = 
  let q = Query "Select * from EVENTS WHERE computername = ? order by id desc limit ? offset ?" :: Query PartialEvent
      pageSize = 20
      params = [toSql cname, toSql pageSize, toSql (pageSize * i)]
   in query q params conn >>= addFilesToEvents conn

queryEventsByIds :: forall eff. Connection -> Array Int -> Aff (db :: DB | eff) (Array Event)
queryEventsByIds conn ids = 
  let is = joinWith "," (show <$> ids)
      q = Query ("Select * from EVENTS WHERE id in (" <> is <> ")") :: Query PartialEvent
   in query_ q conn >>= addFilesToEvents conn

addFilesToEvents :: forall eff. Connection -> Array PartialEvent -> Aff(db :: DB |eff)(Array Event)
addFilesToEvents conn partialEvs = do
  let ids = (\(PartialEvent {id: mi}) -> fromMaybe (-1) mi) <$> partialEvs
  files <- if length ids > -1 then selectFilesForEvents ids conn else return []
  let createEvent pe@(PartialEvent {id: mi}) = mkEvent pe (filter (\(SavedFile im) -> im.eventId == (fromMaybe 0 mi)) files)
  return $ createEvent <$> partialEvs


newPB :: forall eff. Connection -> Photobooth -> Aff (db :: DB | eff) Photobooth
newPB conn pb = do
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

newEvent :: forall eff. Connection -> Event -> Aff (db :: DB | eff) Event
newEvent conn ev = do
    execute_ (insertEvent ev) conn
    res <- queryOne_ (selectLastInserted eventsTable) conn
    case res of
         Nothing -> throwError $ error $ "Failed to create Event"
         Just pe@(PartialEvent {id: Nothing}) -> return $ mkEvent pe []
         Just pe@(PartialEvent {id: Just i}) -> selectFilesForEvent i conn >>= \ims -> return $ mkEvent pe ims

newUser :: User -> Query Unit
newUser (User u) = insert usersTable (fromArray [ Tuple "id" $ show u.id
                                                , Tuple "name" u.name
                                                , Tuple "password" u.password]) false ""


insertEvent :: Event -> Query Unit
insertEvent (Event e) = insert eventsTable
                        (fromArray [ Tuple "computername" e.computername
                                   , Tuple "name" e.name
                                   , Tuple "datefrom" $ toISOString e.datefrom
                                   , Tuple "dateuntil" $ toISOString e.dateuntil
                                   , Tuple "profile" $ e.profile
                                   ]) false ""
                                   
upsertPartialEvent :: PartialEvent -> Query Unit
upsertPartialEvent (PartialEvent e) = insert eventsTable
                        (fromArray [ maybe mempty (\s -> Tuple "id" $ show s) e.id
                                   , Tuple "computername" e.computername
                                   , Tuple "name" e.name
                                   , Tuple "datefrom" $ toISOString e.datefrom
                                   , Tuple "dateuntil" $ toISOString e.dateuntil
                                   , Tuple "profile" $ e.profile
                                   ]) true ""

updatePB :: forall eff. Connection -> Photobooth -> Aff (db :: DB | eff) Photobooth
updatePB _    (Photobooth    {id: Nothing}) =
  throwError $ error $ "updatePhotobooth impossible without id!"
updatePB conn (Photobooth pb@{id: Just i}) =
  let query = update photoboothsTable i (fromArray [Tuple "alias" pb.alias, Tuple "defaultprofile" pb.defaultprofile]) ""
   in do execute_ query conn
         res <- queryOne_ (selectStarId photoboothsTable i) conn
         maybe (throwError $ error $ "Failed to update Photobooth") return res

deletePB :: forall eff. Connection -> String -> Aff (db :: DB | eff) Unit
deletePB c cname = execute (Query "DELETE FROM photobooths where computername = ?") [toSql cname] c
                   *> execute (Query "DELETE FROM events where computername = ?") [toSql cname] c
                   *> execute (Query "DELETE FROM eventstatistics where computername = ?") [toSql cname] c
                   *> execute (Query "DELETE FROM monthlystatistics where computername = ?") [toSql cname] c


updateEvent :: forall eff. Connection -> Event -> Aff (db :: DB | eff) Event
updateEvent _    (Event    {id: Nothing}) =
  throwError $ error $ "updateEvent impossible without id"
updateEvent conn (Event e@{id: Just i}) =
  let query = update eventsTable i (fromArray [ Tuple "computername" e.computername
                                              , Tuple "name" e.name
                                              , Tuple "datefrom" $ toISOString e.datefrom
                                              , Tuple "dateuntil" $ toISOString e.dateuntil
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

saveFileToDb :: forall eff. Connection -> Tuple Int String -> Buffer 
                            -> Aff (db :: DB | eff) SavedFile
saveFileToDb conn (Tuple eventId name) buffer = do
  let query = Query $ "INSERT INTO " <> savedFileTable.name <> " (name, eventid, File) VALUES (?, ?, ?)"
  execute query [toSql name, toSql eventId, toSql buffer] conn
  res <- queryOne_ (selectLastInserted savedFileTable) conn
  maybe (throwError $ error $ "Failed to save File") return res

selectFilesForEvent :: forall eff. Int -> Connection -> Aff (db :: DB | eff) (Array SavedFile)
selectFilesForEvent i conn = query (selectStar savedFileTable "WHERE eventid = ?") [toSql i] conn

selectFilesForEvents :: forall eff. Array Int -> Connection -> Aff (db :: DB | eff) (Array SavedFile)
selectFilesForEvents [] _    = return []
selectFilesForEvents is conn =
  query (selectStar savedFileTable $ "WHERE eventid in (" <> qs <>")") (map toSql is) conn
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

queryNewEvents :: forall eff. Connection -> Tuple String Date -> Aff (db :: DB | eff) (Array PartialEvent)
queryNewEvents conn (Tuple cname d) = do
  query q [toSql cname, toSql $ dateToSqlForComparing d] conn
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

queryNewFiles :: forall eff. Connection -> Tuple String Date -> Aff (db :: DB | eff) (Array SavedFile)
queryNewFiles conn (Tuple cname d) = query q [toSql cname, toSql $ dateToSqlForComparing d] conn
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
  
newtype MyFile = MyFile Buffer

instance foreignMyFile :: IsForeign MyFile where 
  read obj = do
    file <- readProp "file" obj
    return $ MyFile $ unsafeFromForeign file
        
unpack :: MyFile -> Buffer
unpack (MyFile b) = b
  
getFileById :: forall eff. Connection -> Int -> Aff (db :: DB | eff) Buffer
getFileById conn id =
  let q = (Query "select file from files where id = ?" :: Query MyFile)
   in queryOne q [toSql id] conn >>= \mf -> maybe (throwError $ error $ "No file found") (unpack >>> return) mf

updateWorkerState :: forall eff. Connection -> WorkerState -> Aff (db :: DB | eff) Unit 
updateWorkerState c (WorkerState ws) = 
  let q = Query "update WORKERSTATE set activeeventid = ?"
   in execute q [toSql ws.active] c
   
queryActiveEvent :: forall eff. Connection -> Date -> Aff (db :: DB | eff) (Maybe PartialEvent)
queryActiveEvent c d = 
  let q = Query $ "select * from EVENTS where " 
               <> "datefrom <= ? " 
               <> "and dateuntil > ? "
   in queryOne q [toSql $ toISOString d, toSql $ toISOString d] c

getMonthlyStatistic :: forall e. String -> Connection -> Int -> Aff (db :: DB | e) MonthlyStatistic 
getMonthlyStatistic cname c m = 
  let q = Query "select * from monthlystatistics where month = ?"
      handle Nothing = MonthlyStatistic { computername: cname
                                        , month: m
                                        , pictures: 0
                                        , prints: 0}
      handle (Just i) = i
   in queryOne q [toSql m] c >>= (return <<< handle)

tryLogin :: forall e. Connection -> String -> String -> Aff (db :: DB, now :: Now | e) Session
tryLogin conn username password = 
  let userQ = Query "select * from users WHERE name = ? AND password = ?"
      q     = Query "insert into SESSIONS (userId, createdOn) VALUES (?, ?)"
   in do 
     userQueryRes <- (queryOne userQ [toSql username, toSql password] conn)
     (User u) <- maybe (throwError $ error $ "Username or PW not found") return userQueryRes
     d <- liftEff $ now
     execute q [toSql u.id, toSql $ toISOString d] conn
     res <- queryOne_ (selectLastInserted sessionsTable) conn
     maybe (throwError $ error $ "Failed to make new session") return res
