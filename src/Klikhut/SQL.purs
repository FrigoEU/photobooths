module Klikhut.SQL where

import Prelude
import Data.String (joinWith, length)
import Database.AnyDB (Query (..))

data Table = Table String
data FieldDef = FieldDef String FieldType
data FieldType = Char Int

instance showTable :: Show Table where
  show (Table n) = n

instance showFieldDef :: Show FieldDef where
  show (FieldDef name ft) = name ++ " " ++ show ft

instance showFieldType :: Show FieldType where
  show (Char n) = "CHAR(" ++ show n ++ ")"

dropTable :: forall a. Table -> Query a
dropTable (Table s) =  Query $ "DROP TABLE IF EXISTS " ++ s

createTable :: forall a. Table -> Array FieldDef -> Query a
createTable (Table name) fs = Query $ "CREATE TABLE " ++ name ++ " (" ++ (joinWith ", " (map show fs)) ++ ")"

insert :: forall a. Table -> Array String -> Query a
insert (Table name) fs = Query $ "INSERT INTO " ++ name ++ " VALUES (" ++ 
                         joinWith ", " (map (\f -> "'" ++ f ++ "'") fs) ++ ")"

selectStar :: forall a. Table -> String -> Query a
selectStar (Table name) s = Query $ "SELECT * FROM " ++ name ++ " " ++ s
