module App.Model.Date where

import Data.Date (Date)
import Data.Maybe (Maybe(Just, Nothing))

foreign import toISOString :: Date -> String
foreign import toLocalDatetime :: Date -> String
foreign import fromLocalDatetimeImpl :: forall a. Maybe a -> (a -> Maybe a) -> String -> Maybe Date

fromLocalDatetime :: String -> Maybe Date
fromLocalDatetime = fromLocalDatetimeImpl Nothing Just


-- DATES in KLIKHUT
-- ================
-- Dates being added by the triggers are in YYYY-mm-DD HH:MM:SS (remember SQLite has no dates)
-- so dateToSqlForComparing makes a format like that
-- Other dates are being saved in ISOString time (in UTC!)

foreign import dateToSqlForComparing :: Date -> String
