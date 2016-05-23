module App.Model.WorkerState where
  
import Prelude

import SQL as S
import Database.AnyDB (Query (..))
import Database.AnyDB.SqlValue
  
import Data.Generic
import Data.Foreign.Class
import Data.Tuple

import App.Model.StrMap (fromArray)

data Active = DefaultActive | EventActive Int
data WorkerState = WorkerState { active :: Active }

derive instance genericActive :: Generic Active
derive instance genericWorkerState :: Generic WorkerState

instance eqActive   :: Eq Active where eq = gEq
instance showActive :: Show Active where show = gShow
instance eqWorkerState   :: Eq WorkerState where eq = gEq
instance showWorkerState :: Show WorkerState where show = gShow

instance foreignActive :: IsForeign Active where
  read obj = do 
    i :: Int <- read obj
    if i == -1 then return DefaultActive
               else return $ EventActive i
               
instance isSqlValueActive :: IsSqlValue Active where
  toSql DefaultActive = toSql (-1 :: Int)
  toSql (EventActive i) = toSql i

instance foreignWorkerState :: IsForeign WorkerState where 
  read obj = do
    active <- readProp "activeeventid" obj
    return $ WorkerState { active }

workerStateTable :: S.Table
workerStateTable = { name: "WORKERSTATE"
                   , columns: fromArray [ Tuple "activeeventid" $ S.ColumnDef S.Integer []]}

createWorkerStateTable :: Query Unit
createWorkerStateTable = Query "CREATE TABLE WORKERSTATE (activeeventid INTEGER);"
