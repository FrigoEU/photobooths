module Test.Klikhut.SQL where

import Prelude
import Klikhut.SQL
import Database.AnyDB (Query (..), withConnection, query_, execute_, ConnectionInfo(..))
import Klikhut.Model.Photobooth as PB
import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)

import Test.Spec                  (describe, pending, it)
import Test.Spec.Assertions       (shouldEqual)

connectionInfo = Sqlite3  
  { filename: "test"
  , memory: true }

mybooth = PB.Photobooth { computername: "mycomputername"
                        , alias: "myalias"
                        , defaultprofile: "myprofile" }

main = do
  describe "integration test Sqlite3 + Photobooth type" do
    it "should make a db, insert a Photobooth row, and get it back out" do
        withConnection connectionInfo \conn -> do 
          execute_ (createTable PB.table PB.cols) conn
          execute_ (dropTable PB.table) conn
          execute_ (createTable PB.table PB.cols) conn
          (PB.Photobooth m) <- return mybooth
          execute_ (insert PB.table [m.computername, m.alias, m.defaultprofile]) conn
          result <- query_ (selectStar PB.table "" :: Query PB.Photobooth) conn
          result `shouldEqual` [mybooth]
  describe "createTable" do
    it "should make the correct SQL string" do
      createTable (Table "mytable") [FieldDef "mycol1" $ Char 20, FieldDef "mycol2" $ Char 24]
        `shouldEqual`
        Query "CREATE TABLE mytable (mycol1 CHAR(20), mycol2 CHAR(24))"
  describe "dropTable" do
    it "dropTable makes the correct SQL string" do
      dropTable (Table "mytable") 
        `shouldEqual`
        Query "DROP TABLE IF EXISTS mytable"
  describe "insert" do
    it "insert makes the correct SQL string" do
      insert (Table "mytable") ["ab", "cd"]
        `shouldEqual`
        Query "INSERT INTO mytable VALUES ('ab', 'cd')"
  describe "selectStar" do
    it "selectStart should make the correct SQL" do
      selectStar (Table "mytable") "where name = 'ab'"
        `shouldEqual`
        Query "SELECT * FROM mytable where name = 'ab'"
