module Test.App.SQL where

import Prelude
import Database.AnyDB (Query (..), withConnection, query_, execute_, ConnectionInfo(..))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)

import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.Foreign
import Data.Foreign.Class
import Data.Generic

import SQL
import App.Model.Photobooth 
import App.Model.StrMap (fromArray)

import Test.Spec                  (describe, pending, it)
import Test.Spec.Assertions       (shouldEqual)

connectionInfo = Sqlite3  
  { filename: "test"
  , memory: true }

data MyId = MyId {id :: Int}
derive instance genericId :: Generic MyId
instance isForeignId :: IsForeign MyId where
  read obj = do
    id <- readProp "id" obj
    return $ MyId {id: id}

instance eqId :: Eq MyId 
  where eq = gEq
instance showId :: Show MyId 
  where show = gShow

mybooth = Photobooth { id: Just 1
                        , computername: "mycomputername"
                        , alias: "myalias"
                        , defaultprofile: "myprofile" }

main = do
  describe "integration test Sqlite3 + Photobooth type" do
    it "should make a db, insert a Photobooth row, and get it back out" do
        withConnection connectionInfo \conn -> do 
          execute_ (createTable photoboothsTable) conn
          execute_ (dropTable photoboothsTable) conn
          execute_ (createTable photoboothsTable) conn
          (Photobooth m) <- return mybooth
          execute_ (insert photoboothsTable (fromArray [ Tuple "id" $ show $ maybe 0 id m.id 
                                                       , Tuple "computername" m.computername
                                                       , Tuple "alias" m.alias
                                                       , Tuple "defaultprofile" m.defaultprofile]) false "") conn
          result <- query_ (selectStar photoboothsTable "" :: Query Photobooth) conn
          result `shouldEqual` [mybooth]

  describe "integration inserting integer values" do
    it "should insert and extract integer values correctly" do
        withConnection connectionInfo \conn -> do
          let t = {name: "mytable", columns: fromArray [Tuple "id" $ ColumnDef Integer []]}
          execute_ (createTable t) conn
          execute_ (insert t (fromArray [Tuple "id" $ show 5]) false "") conn
          result <- query_ (selectStar t "" :: Query MyId) conn
          result `shouldEqual` [MyId {id: 5}]

  describe "createTable" do
    it "should make the correct SQL string" do
      createTable {name: "mytable", columns: fromArray [Tuple "mycol1" $ ColumnDef Char [], Tuple "mycol2" $ ColumnDef Char []]} 
        `shouldEqual`
        Query "CREATE TABLE mytable (mycol1 CHAR, mycol2 CHAR)"
    it "should make the correct Primary Key string" do
      createTable {name: "mytable", columns: fromArray [Tuple "mycol3" $ ColumnDef Integer [PrimaryKey], Tuple "mycol4" $ ColumnDef Char []]}
        `shouldEqual`
        Query "CREATE TABLE mytable (mycol3 INTEGER PRIMARY KEY, mycol4 CHAR)"

  describe "dropTable" do
    it "dropTable makes the correct SQL string" do
      dropTable {name: "mytable", columns: SM.empty} 
        `shouldEqual`
        Query "DROP TABLE IF EXISTS mytable"

  describe "insert" do
    it "insert makes the correct SQL string" do
      insert {name: "mytable", columns: fromArray [Tuple "mycol1" $ ColumnDef Char [], Tuple "mycol2" $ ColumnDef Char []]} 
             (fromArray [Tuple "mycol1" "ab", Tuple "mycol2" "cd"]) 
             false
             ""
        `shouldEqual`
        Query "INSERT INTO mytable (mycol1, mycol2) VALUES ('ab', 'cd');"

  describe "insertGet" do
    it "insertGet makes the correct SQL string" do
      insertGet {name: "mytable", columns: fromArray [Tuple "mycol1" $ ColumnDef Char [], Tuple "mycol2" $ ColumnDef Char []]} 
                (fromArray [Tuple "mycol2" "cd"])
        `shouldEqual`
        Query ("INSERT INTO mytable (mycol2) VALUES ('cd');" ++ 
               " SELECT * FROM mytable WHERE id = last_insert_rowid();")

  describe "update" do
    it "update makes the correct SQL string" do
      update {name: "mytable", columns: fromArray [Tuple "mycol1" $ ColumnDef Char [], Tuple "mycol2" $ ColumnDef Char [], Tuple "mycol3" $ ColumnDef Integer []]}
                1 
                (fromArray [Tuple "mycol2" "cd", Tuple "mycol3" $ show 2]) ""
        `shouldEqual`
        Query ("UPDATE mytable SET mycol2 = 'cd', mycol3 = '2' WHERE id = 1")

  describe "updateGet" do
    it "updateGet makes the correct SQL string" do
      updateGet {name: "mytable", columns: fromArray [Tuple "mycol1" $ ColumnDef Char [], Tuple "mycol2" $ ColumnDef Char [], Tuple "mycol3" $ ColumnDef Integer []]}
                1 
                (fromArray [Tuple "mycol2" "cd", Tuple "mycol3" $ show 2])
        `shouldEqual`
        Query ("UPDATE mytable SET mycol2 = 'cd', mycol3 = '2' WHERE id = 1;" ++ 
               " SELECT * FROM mytable WHERE id = 1;")

  describe "selectStar" do
    it "selectStar should make the correct SQL" do
      selectStar {name: "mytable", columns: SM.empty} "where name = 'ab'"
        `shouldEqual`
        Query "SELECT * FROM mytable where name = 'ab'"

  describe "lastInserted" do
    it "selectLastInserted should make the correct SQL" do
      selectLastInserted {name: "mytable", columns: SM.empty}
        `shouldEqual`
        Query "SELECT * FROM mytable WHERE id = last_insert_rowid()"
