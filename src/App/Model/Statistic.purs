module App.Model.Statistic where

import Prelude

import SQL as S

import Data.Foreign
import Data.Foreign.Class
import Data.Generic
import Data.Lens (Lens(), lens)
import Data.Tuple (Tuple(..))
import Data.Maybe

import App.Model.StrMap

data EventStatistic = EventStatistic { photoboothId :: Int
                                     , eventId :: Int
                                     , pictures :: Int
                                     , prints :: Int }

derive instance genericEventStatistic :: Generic EventStatistic

instance eqEventStatistic         :: Eq EventStatistic where eq = gEq 
instance showEventStatistic       :: Show EventStatistic where show = gShow

instance eventStatisticIsForeign :: IsForeign EventStatistic where
  read obj = do
    photoboothId <- readProp "photoboothId" obj
    eventId <- readProp "eventId" obj
    pictures <- readProp "pictures" obj
    prints <- readProp "prints" obj
    return $ EventStatistic { photoboothId: photoboothId, eventId: eventId
                            , pictures: pictures, prints: prints }


eventStatisticsTable = { name: "EVENTSTATISTICS" 
                       , columns: fromArray [ Tuple "photoboothId" $ S.ColumnDef S.Integer []
                                            , Tuple "eventId" $ S.ColumnDef S.Integer []
                                            , Tuple "pictures" $ S.ColumnDef S.Integer []
                                            , Tuple "prints" $ S.ColumnDef S.Integer []
                                            ]}

_EventStatistic = lens  (\(EventStatistic a) -> a) (\_ a -> EventStatistic a)
createEventStatisticsTable = "CREATE TABLE EVENTSTATISTICS (photoboothId INTEGER, eventId INTEGER, pictures INTEGER, prints INTEGER, PRIMARY KEY (photoboothId, eventId))"

data MonthlyStatistic = MonthlyStatistic { photoboothId :: Int
                                         , month :: Int
                                         , pictures :: Int
                                         , prints :: Int }

derive instance genericMonthlyStatistic :: Generic MonthlyStatistic

instance eqMonthlyStatistic         :: Eq MonthlyStatistic where eq = gEq 
instance showMonthlyStatistic       :: Show MonthlyStatistic where show = gShow

instance monthlyStatisticIsForeign :: IsForeign MonthlyStatistic where
  read obj = do
    photoboothId <- readProp "photoboothId" obj
    month <- readProp "month" obj
    pictures <- readProp "pictures" obj
    prints <- readProp "prints" obj
    return $ MonthlyStatistic { photoboothId: photoboothId, month: month
                              , pictures: pictures, prints: prints }


monthlyStatisticsTable = { name: "MONTHLYSTATISTICS" 
                         , columns: fromArray [ Tuple "photoboothId" $ S.ColumnDef S.Integer []
                                              , Tuple "month" $ S.ColumnDef S.Integer []
                                              , Tuple "pictures" $ S.ColumnDef S.Integer []
                                              , Tuple "prints" $ S.ColumnDef S.Integer []
                                              ]}


createMonthlyStatisticsTable = "CREATE TABLE MONTHLYSTATISTICS (photoboothId INTEGER, month INTEGER, pictures INTEGER, prints INTEGER, PRIMARY KEY (photoboothId, month))"
_MonthlyStatistic = lens  (\(MonthlyStatistic a) -> a) (\_ a -> MonthlyStatistic a)
