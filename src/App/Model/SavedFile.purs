module App.Model.SavedFile where

import Prelude

import SQL as S

import Data.Foreign.Class
import Data.Generic
import Data.Tuple (Tuple(..))

import App.Model.StrMap (fromArray)

data SavedFile = SavedFile { id :: Int
                           , name :: String
                           , eventId :: Int }

derive instance genericSavedFile :: Generic SavedFile

instance eqSavedFile         :: Eq SavedFile where eq = gEq 
instance showSavedFile       :: Show SavedFile where show = gShow

instance savedFileIsForeign :: IsForeign SavedFile where
  read obj = do
    id <- readProp "id" obj
    name <- readProp "name" obj
    eventId <- readProp "eventid" obj
    return $ SavedFile {id, name, eventId}

savedFileTable = { name: "FILES"
                  , columns: fromArray [ Tuple "id" $ S.ColumnDef S.Integer [S.PrimaryKey]
                                       , Tuple "name" $ S.ColumnDef S.Char []
                                       , Tuple "eventid" $ S.ColumnDef S.Integer []
                                       , Tuple "file" $ S.ColumnDef S.Blob []
                                       , Tuple "updatedon" $ S.ColumnDef S.Date []
                                       ]}
