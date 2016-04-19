module App.Model.User where

import Prelude (($), return, bind)

import SQL as S

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Tuple (Tuple(Tuple))

import App.Model.StrMap (fromArray)

------------------------------------------

data User = User { id :: Int
                 , name :: String
                 , password :: String }


instance userIsForeign :: IsForeign User where
  read obj = do
    id <- readProp "id" obj
    name <- readProp "name" obj
    password <- readProp "password" obj
    return $ User {id, name, password}

usersTable :: S.Table 
usersTable = { name: "USERS" 
             , columns: fromArray [ Tuple "id" $ S.ColumnDef S.Integer [S.PrimaryKey]
                                  , Tuple "name" $ S.ColumnDef S.Char []
                                  , Tuple "password" $ S.ColumnDef S.Char [] 
                                  ]}
