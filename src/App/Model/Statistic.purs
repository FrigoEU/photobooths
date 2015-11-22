module App.Model.Statistic where

import Prelude

import SQL as S

import Data.Foreign
import Data.Foreign.Class
import Data.Generic
import Data.Lens (Lens(), lens)
import Data.Tuple (Tuple(..))
import Data.Maybe
import Data.Date

import App.Model.StrMap

data EventStatistic = EventStatistic { computername :: String
                                     , eventId :: Int
                                     , pictures :: Int
                                     , prints :: Int }

derive instance genericEventStatistic :: Generic EventStatistic

instance eqEventStatistic         :: Eq EventStatistic where eq = gEq 
instance showEventStatistic       :: Show EventStatistic where show = gShow

instance eventStatisticIsForeign :: IsForeign EventStatistic where
  read obj = do
    computername <- readProp "computername" obj
    eventId <- readProp "eventId" obj
    pictures <- readProp "pictures" obj
    prints <- readProp "prints" obj
    return $ EventStatistic { computername, eventId, pictures, prints }


eventStatisticsTable = { name: "EVENTSTATISTICS" 
                       , columns: fromArray [ Tuple "computername" $ S.ColumnDef S.Char []
                                            , Tuple "eventId" $ S.ColumnDef S.Integer []
                                            , Tuple "pictures" $ S.ColumnDef S.Integer []
                                            , Tuple "prints" $ S.ColumnDef S.Integer []
                                            ]}

_EventStatistic = lens  (\(EventStatistic a) -> a) (\_ a -> EventStatistic a)
createEventStatisticsTable = "CREATE TABLE EVENTSTATISTICS (computername CHAR, eventId INTEGER, pictures INTEGER, prints INTEGER, PRIMARY KEY (computername, eventId))"

data MonthlyStatistic = MonthlyStatistic { computername :: String
                                         , month :: Int
                                         , pictures :: Int
                                         , prints :: Int }

derive instance genericMonthlyStatistic :: Generic MonthlyStatistic

instance eqMonthlyStatistic         :: Eq MonthlyStatistic where eq = gEq 
instance showMonthlyStatistic       :: Show MonthlyStatistic where show = gShow

instance monthlyStatisticIsForeign :: IsForeign MonthlyStatistic where
  read obj = do
    computername <- readProp "computername" obj
    month <- readProp "month" obj
    pictures <- readProp "pictures" obj
    prints <- readProp "prints" obj
    return $ MonthlyStatistic { computername, month, pictures, prints }


monthlyStatisticsTable = { name: "MONTHLYSTATISTICS" 
                         , columns: fromArray [ Tuple "computername" $ S.ColumnDef S.Integer []
                                              , Tuple "month" $ S.ColumnDef S.Integer []
                                              , Tuple "pictures" $ S.ColumnDef S.Integer []
                                              , Tuple "prints" $ S.ColumnDef S.Integer []
                                              ]}


createMonthlyStatisticsTable = "CREATE TABLE MONTHLYSTATISTICS (computername CHAR, month INTEGER, pictures INTEGER, prints INTEGER, PRIMARY KEY (computername, month))"
_MonthlyStatistic = lens  (\(MonthlyStatistic a) -> a) (\_ a -> MonthlyStatistic a)

data AllStatistics = AllStatistics { eventStatistics :: Array EventStatistic
                                   , monthlyStatistics :: Array MonthlyStatistic}

derive instance genericAllStatistics :: Generic AllStatistics
instance eqAllStatistics         :: Eq AllStatistics where eq = gEq
instance showAllStatistics       :: Show AllStatistics where show = gShow

getMonthText :: Int -> String
getMonthText 1 = "Januari"
getMonthText 2 = "Februari"
getMonthText 3 = "Maart"
getMonthText 4 = "April"
getMonthText 5 = "Mei"
getMonthText 6 = "Juni"
getMonthText 7 = "Juli"
getMonthText 8 = "Augustus"
getMonthText 9 = "September"
getMonthText 10 = "October"
getMonthText 11 = "November"
getMonthText 12 = "December"
getMonthText _ = "Foute maand"

monthToInt :: Month -> Int
monthToInt January = 1
monthToInt February = 2
monthToInt March = 3
monthToInt April = 4
monthToInt May = 5
monthToInt June = 6
monthToInt July = 7
monthToInt August = 8
monthToInt September = 9
monthToInt October = 10
monthToInt November = 11
monthToInt December = 12
