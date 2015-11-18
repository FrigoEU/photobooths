module Test.App.Model.Photobooth where

import Prelude
import App.Model.Photobooth
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Either
import Data.Maybe

import Test.Spec                  (describe, it)
import Test.Spec.Assertions       (shouldEqual)

mybooth = Photobooth { id: Nothing
                     , computername: "mycomputername"
                     , alias: "myalias"
                     , defaultprofile: "myprofile" }

mybooth2 = Photobooth { id: Just 1
                      , computername: "mycomputername"
                      , alias: "myalias"
                      , defaultprofile: "myprofile2" }

main = do
  describe "json" do
    it "encodeJson and decodeJson are inverses 1" do
      ((decodeJson <<< encodeJson) mybooth :: Either String Photobooth) 
        `shouldEqual` 
        Right mybooth
    it "encodeJson and decodeJson are inverses 2" do
      ((decodeJson <<< encodeJson) mybooth2 :: Either String Photobooth) 
        `shouldEqual` 
        Right mybooth2
