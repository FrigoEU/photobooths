module App.Model.Date where

import Data.Date (Date)
import Data.Maybe (Maybe(Just, Nothing))

foreign import toISOString :: Date -> String
foreign import toLocalDatetime :: Date -> String
foreign import fromLocalDatetimeImpl :: forall a. Maybe a -> (a -> Maybe a) -> String -> Maybe Date

fromLocalDatetime :: String -> Maybe Date
fromLocalDatetime = fromLocalDatetimeImpl Nothing Just
