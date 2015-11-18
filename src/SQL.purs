module SQL where

import Prelude
import Data.String (joinWith, trim) 
import Database.AnyDB (Query (..))
import Data.Array (zip, sort, sortBy, length)
import Data.Tuple (Tuple(..), fst, snd)
import Data.StrMap (StrMap(), keys)

import App.Model.StrMap (toArray)

---------------------------------

type Table = { name :: String
             , columns :: StrMap ColumnDef
             }

data ColumnDef = ColumnDef FieldType (Array ColumnConstraint)

data ColumnConstraint = PrimaryKey
                      | Unique

data FieldType = Char
               | Integer
               | Date
               | Blob

instance showColumnDef :: Show ColumnDef where
  show (ColumnDef ft ccs) = show ft ++ (if length ccs > 0 then " " else "") ++ 
                              joinWith ", " (map show ccs)

instance showFieldType :: Show FieldType where
  show Char = "CHAR"
  show Integer  = "INTEGER"
  show Date  = "CHAR"
  show Blob  = "BLOB"

instance showColumnConstraint :: Show ColumnConstraint where
  show PrimaryKey = "PRIMARY KEY"
  show Unique = "UNIQUE"

dropTable :: forall a. Table -> Query a
dropTable {name: s} =  Query $ "DROP TABLE IF EXISTS " ++ s

createColumnDefs :: StrMap ColumnDef -> String
createColumnDefs s = joinWith ", " $ map (\(Tuple name cdef) -> name ++ " " ++ show cdef ) $ toArray s

createTable :: forall a. Table -> Query a
createTable {name: s, columns: fs} = Query $ "CREATE TABLE " ++ s ++ parens (createColumnDefs fs)

insert :: forall a . Table -> StrMap String -> String -> Query a
insert {name: name, columns: fds} fs str = 
  let sortedKeys = sort $ keys fs
      sortedValues = (map snd <<<
                      sortBy (\f1 f2 -> compare (fst f1) (fst f2)) <<< 
                      toArray)
                      fs
  in Query $ "INSERT INTO " ++ name ++ 
             parens (joinWith ", " sortedKeys) ++
             " VALUES" ++ 
             parens (joinWith ", " $ (map \str -> "'" ++ str ++ "'") sortedValues) ++ ";" ++
             str


insertGet :: forall a. Table -> StrMap String -> Query a
insertGet t@({name: name}) fs = insert t fs $
                                " SELECT * FROM " ++ name ++ 
                                " WHERE id = last_insert_rowid();"

update :: forall a. Table -> Int -> StrMap String -> String -> Query a
update {name: name, columns: fds} i fs str =
  let sortedKeys = sort $ keys fs
      sortedValues = (map snd <<<
                      sortBy (\f1 f2 -> compare (fst f1) (fst f2)) <<< 
                      toArray
                      ) fs
      fields = zip sortedKeys sortedValues
   in Query $ "UPDATE " ++ name ++ " SET " ++
              (joinWith ", " (map (\(Tuple fd f) -> fd ++ " = '" ++ f ++"'") fields)) ++
              " WHERE id = " ++ show i  ++ str

updateGet :: forall a. Table -> Int -> StrMap String -> Query a
updateGet t@{name: name} i fs = update t i fs $ "; SELECT * FROM " ++ name ++ " WHERE id = " ++ show i ++ ";"


selectStar :: forall a. Table -> String -> Query a
selectStar {name: name} s = Query $ "SELECT * FROM " ++ name ++ " " ++ s

selectStarId :: forall a. Table -> Int -> Query a
selectStarId t i = selectStar t $ "WHERE id = " ++ show i

selectLastInserted :: forall a. Table -> Query a
selectLastInserted t = selectStar t "WHERE id = last_insert_rowid()"

parens :: String -> String
parens s = " (" ++ s ++ ")"
