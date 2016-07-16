module App.Model.Date where

import Data.Date (Date)
import Data.Maybe (Maybe(Just, Nothing))

foreign import toISOString :: Date -> String
foreign import toLocalDatetime :: Date -> String
foreign import fromLocalDatetimeImpl :: forall a. Maybe a -> (a -> Maybe a) -> String -> Maybe Date

fromLocalDatetime :: String -> Maybe Date
fromLocalDatetime = fromLocalDatetimeImpl Nothing Just

-- This is still using the old datetime, comparing with dates seems to be going badly
-- so I added another function
foreign import dateToSqlForComparing :: Date -> String
