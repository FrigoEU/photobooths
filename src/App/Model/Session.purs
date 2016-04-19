module App.Model.Session where

import Prelude (($), return, bind)

import SQL as S

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign (ForeignError(TypeMismatch))
import Data.Generic (class Generic)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe)
import Data.Either (Either(Right, Left))
import Data.Date (Date, fromStringStrict)

import App.Model.StrMap (fromArray)

------------------------------------------

data Session = Session { id :: Int
                       , userId :: Int 
                       , createdOn :: Date}

derive instance genericSession :: Generic Session

instance sessionIsForeign :: IsForeign Session where
  read obj = do
    id <- readProp "id" obj
    userId <- readProp "userId" obj
    createdOnF <- readProp "createdOn" obj
    createdOn <- maybe (Left $ TypeMismatch "ISO 8601 Date" createdOnF) 
                        Right 
                       (fromStringStrict createdOnF)
    return $ Session {id, userId, createdOn}

sessionsTable :: S.Table 
sessionsTable = { name: "SESSIONS" 
                , columns: fromArray [ Tuple "id" $ S.ColumnDef S.Integer [S.PrimaryKey]
                                     , Tuple "userId" $ S.ColumnDef S.Integer []
                                     , Tuple "createdOn" $ S.ColumnDef S.Date []
                                     ]}
