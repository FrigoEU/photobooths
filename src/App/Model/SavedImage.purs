module App.Model.SavedImage where

import Prelude

import SQL as S

import Data.Foreign
import Data.Foreign.Class
import Data.Generic
import Data.Lens (Lens(), lens)
import Data.Tuple (Tuple(..))

import App.Model.StrMap (fromArray)

data SavedImage = SavedImage { id :: Int
                             , name :: String
                             , eventId :: Int }

derive instance genericSavedImage :: Generic SavedImage

instance eqSavedImage         :: Eq SavedImage where eq = gEq 
instance showSavedImage       :: Show SavedImage where show = gShow

instance savedImageIsForeign :: IsForeign SavedImage where
  read obj = do
    id <- readProp "id" obj
    name <- readProp "name" obj
    eventId <- readProp "eventid" obj
    return $ SavedImage {id: id, name: name, eventId: eventId}

savedImageTable = { name: "IMAGES"
                  , columns: fromArray [ Tuple "id" $ S.ColumnDef S.Integer [S.PrimaryKey]
                                       , Tuple "name" $ S.ColumnDef S.Char []
                                       , Tuple "eventid" $ S.ColumnDef S.Integer []
                                       , Tuple "image" $ S.ColumnDef S.Blob []]}
