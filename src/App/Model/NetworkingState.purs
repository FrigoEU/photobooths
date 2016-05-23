module App.Model.NetworkingState where
  
import Prelude (class Show, class Eq, Unit, ($), return, bind)
import Data.Date (Date, fromStringStrict)
import Data.Generic (class Generic, gShow, gEq)
import Data.Foreign (ForeignError(TypeMismatch))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Tuple (Tuple(Tuple))
import Data.Maybe (maybe)
import Data.Either (Either(Right, Left))
import SQL as S
import App.Model.StrMap (fromArray)
import Database.AnyDB (Query(..))

data NetworkingState = NetworkingState { lastSentStatistics :: Date }

derive instance genericNetworkingState :: Generic NetworkingState

instance eqNetworkingState   :: Eq NetworkingState where eq = gEq
instance showNetworkingState :: Show NetworkingState where show = gShow

instance foreignNetworkingState :: IsForeign NetworkingState where 
  read obj = do
    lastSentStatisticsF <- readProp "lastsentstatistics" obj
    lastSentStatistics <- maybe (Left $ TypeMismatch "ISO 8601 Date" lastSentStatisticsF) Right 
                          $ fromStringStrict lastSentStatisticsF
    return $ NetworkingState { lastSentStatistics }

networkingStateTable :: S.Table
networkingStateTable = { name: "NETWORKINGSTATE"
                       , columns: fromArray [ Tuple "lastsentstatistics" $ S.ColumnDef S.Date []]}

createNetworkingStateTable :: Query Unit
createNetworkingStateTable = Query "CREATE TABLE NETWORKINGSTATE (lastsentstatistics CHAR);"

insertDefaultNetworkingSate :: Query Unit
insertDefaultNetworkingSate = Query "INSERT INTO NETWORKINGSTATE (lastsentstatistics) VALUES (date('now'))"
