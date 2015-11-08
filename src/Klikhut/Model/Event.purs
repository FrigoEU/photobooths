module Klikhut.Model.Event where

import Prelude
import Klikhut.SQL
import Data.Foreign.Class
import Data.Generic
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Lens (Lens(), lens)
import Data.Date
import Data.Time
import Data.Maybe

data Event = Event { id :: Maybe Int
                   , computername :: String
                   , name :: String
                   , from :: Date
                   , until :: Date
                   , profile :: String }

derive instance genericEvent :: Generic Event

instance encodeJsonEvent :: EncodeJson Event where encodeJson = gEncodeJson
instance decodeJsonEvent :: DecodeJson Event where decodeJson = gDecodeJson
instance eqEvent         :: Eq Event where eq = gEq 
instance showEvent       :: Show Event where show = gShow


{-- newtype AppDate = AppDate Date --}

{-- --foreign import appDateToISO :: AppDate -> String --}
{-- instance genericAppDate :: Generic AppDate where --}
{--   toSpine (AppDate d) = case toEpochMilliseconds d of --} 
{--                              (Milliseconds n) -> SNumber n --}
{--   toSignature (Proxy ) --}

