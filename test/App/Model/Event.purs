module Test.App.Model.Event where

import Prelude

import Test.Spec                  (describe, it)
import Test.Spec.Assertions       (shouldEqual)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff

import Data.Date (now, Now(), Date())
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Either
import Data.Maybe

import App.Model.Event
import App.Model.SavedImage

myEventMaker d = Event { id: Just 1
                       , computername: "mycomputername"
                       , name: "myname"
                       , datefrom: d
                       , dateuntil: d
                       , profile: "myprofile" 
                       , images: [SavedImage {id: 1, name: "yup.jpg", eventId: 1}]}

main = do
  describe "Event json encoding & decoding" do
    it "encodeJson and decodeJson are inverses" do
      n <- (liftEff now :: forall eff. Aff (now :: Now | eff) Date)
      let myEvent = myEventMaker n
          coded = ((decodeJson <<< encodeJson) myEvent :: Either String Event)
      shouldEqual coded (Right myEvent)
