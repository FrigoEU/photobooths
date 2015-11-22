module App.Model.Photobooth where

import Prelude

import SQL

import Data.Foreign.Class
import Data.Generic
import Data.Maybe (Maybe(..))
import Data.Lens (Lens(), lens)
import Data.Tuple (Tuple(..))

import App.Model.StrMap

data Photobooth = Photobooth { id             :: Maybe Int
                             , computername   :: String
                             , alias          :: String
                             , defaultprofile :: String }

derive instance genericPhotobooth :: Generic Photobooth

instance eqPhotobooth         :: Eq Photobooth where eq = gEq 
instance showPhotobooth       :: Show Photobooth where show = gShow

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
                                        ]}

_Photobooth = lens  (\(Photobooth a) -> a) (\_ a -> Photobooth a)
