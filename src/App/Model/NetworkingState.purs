module App.Model.NetworkingState where
  
import Prelude

import SQL as S
import Database.AnyDB (Query (..))
  
import Data.Date
import Data.Generic
import Data.Foreign
import Data.Foreign.Class
import Data.Tuple
import Data.Maybe
import Data.Either

import App.Model.StrMap (fromArray)

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
createNetworkingStateTable = Query "CREATE TABLE NETWORKINGSTATE (lastsentstatistics CHAR); INSERT INTO NETWORKINGSTATE (lastsentstatistics) VALUES (date('now'))"
