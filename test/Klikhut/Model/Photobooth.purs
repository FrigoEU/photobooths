module Test.Klikhut.Model.Photobooth where

import Prelude
import Klikhut.Model.Photobooth
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Either

import Test.Spec                  (describe, it)
import Test.Spec.Assertions       (shouldEqual)

mybooth = Photobooth { computername: "mycomputername"
                     , alias: "myalias"
                     , defaultprofile: "myprofile" }

mybooth2 = Photobooth { computername: "mycomputername"
                     , alias: "myalias"
                     , defaultprofile: "myprofile2" }

main = do
  describe "json" do
    it "encodeJson and decodeJson are inverses" do
      ((decodeJson <<< encodeJson) mybooth :: Either String Photobooth) 
        `shouldEqual` 
        Right mybooth
