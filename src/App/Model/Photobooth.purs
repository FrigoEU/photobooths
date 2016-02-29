module App.Model.Photobooth where

import Prelude

import SQL

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Generic (class Generic, gShow, gEq)
import Data.Maybe (Maybe(Just), maybe)
import Data.Lens (lens)
import Data.Tuple (Tuple(..))
import Data.Array (sortBy)

import App.Model.StrMap

data Photobooth = Photobooth { id             :: Maybe Int
                             , computername   :: String
                             , alias          :: String
                             , defaultprofile :: String }

derive instance genericPhotobooth :: Generic Photobooth

instance eqPhotobooth         :: Eq Photobooth where eq = gEq 
instance showPhotobooth       :: Show Photobooth where show = gShow

sortPhotobooths :: Array Photobooth -> Array Photobooth
sortPhotobooths = sortBy (\(Photobooth {id: mi1}) (Photobooth {id: mi2}) 
                       -> compare (maybe 999999 id mi2) (maybe 999999 id mi1))


instance photoboothIsForeign :: IsForeign Photobooth where
  read obj = do
    id <- Just <$> readProp "id" obj
    computername <- readProp "computername" obj
    alias <- readProp "alias" obj
    defaultprofile <- readProp "defaultprofile" obj
    return $ Photobooth {id, computername, alias, defaultprofile}

photoboothsTable = { name: "PHOTOBOOTHS"
                   , columns: fromArray [ Tuple "id" $ ColumnDef Integer [PrimaryKey]
                                        , Tuple "computername" $ ColumnDef Char [Unique]
                                        , Tuple "alias" $ ColumnDef Char []
                                        , Tuple "defaultprofile" $ ColumnDef Char []
                                        , Tuple "updatedon" $ ColumnDef Date []
                                        ]}

_Photobooth = lens  (\(Photobooth a) -> a) (\_ a -> Photobooth a)
