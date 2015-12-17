module App.Model.Event where

import Prelude

import SQL as S

import Data.Foreign
import Data.Foreign.Class
import Data.Generic
import Data.Lens (lens)
import Data.Tuple (Tuple(..))
import Data.Date
import Data.Maybe
import Data.Either

import App.Model.StrMap (fromArray)
import App.Model.SavedFile

------------------------------------------

data Event = Event { id :: Maybe Int
                   , computername :: String
                   , name :: String
                   , datefrom :: Date
                   , dateuntil :: Date
                   , profile :: String 
                   , files :: Array SavedFile}

data PartialEvent = PartialEvent { id :: Maybe Int
                                 , computername :: String
                                 , name :: String
                                 , datefrom :: Date
                                 , dateuntil :: Date
                                 , profile :: String }

mkEvent :: PartialEvent -> Array SavedFile -> Event
mkEvent (PartialEvent pe) im = Event { id: pe.id
                                     , computername: pe.computername
                                     , name: pe.name
                                     , datefrom: pe.datefrom
                                     , dateuntil: pe.dateuntil
                                     , profile: pe.profile 
                                     , files: im}

derive instance genericEvent :: Generic Event
derive instance genericPartialEvent :: Generic PartialEvent

instance eqEvent         :: Eq Event where eq = gEq 
instance showEvent       :: Show Event where show = gShow

instance partialEventIsForeign :: IsForeign PartialEvent where
  read obj = do
    id <- Just <$> readProp "id" obj
    computername <- readProp "computername" obj
    name <- readProp "name" obj
    datefromF <- readProp "datefrom" obj
    datefrom <- maybe (Left $ TypeMismatch "ISO 8601 Date" datefromF) Right 
                  $ fromStringStrict datefromF
    dateuntilF <- readProp "dateuntil" obj
    dateuntil <- maybe (Left $ TypeMismatch "ISO 8601 Date" dateuntilF) Right 
                   $ fromStringStrict dateuntilF
    profile <- readProp "profile" obj
    return $ PartialEvent {id, computername, name, datefrom, dateuntil, profile}

eventsTable :: S.Table 
eventsTable = { name: "EVENTS" 
              , columns: fromArray [ Tuple "id" $ S.ColumnDef S.Integer [S.PrimaryKey]
                                   , Tuple "computername" $ S.ColumnDef S.Char []
                                   , Tuple "name" $ S.ColumnDef S.Char []
                                   , Tuple "datefrom" $ S.ColumnDef S.Date []
                                   , Tuple "dateuntil" $ S.ColumnDef S.Date []
                                   , Tuple "profile" $ S.ColumnDef S.Char []
                                   , Tuple "updatedon" $ S.ColumnDef S.Date []
                                   ]}

_Event = lens  (\(Event a) -> a) (\_ a -> Event a)
_PartialEvent = lens  (\(PartialEvent a) -> a) (\_ a -> PartialEvent a)
