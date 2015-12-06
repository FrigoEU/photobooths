module Test.App.Model.Event where

import Prelude

import Test.Spec                  (describe, it)
import Test.Spec.Assertions       (shouldEqual)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff

import Data.Foreign.Generic
import Data.Foreign
import Data.Date (now, Now(), Date())
import Data.Either
import Data.Maybe

import App.Model.Event
import App.Model.SavedFile

myEventMaker d = Event { id: Just 1
                       , computername: "mycomputername"
                       , name: "myname"
                       , datefrom: d
                       , dateuntil: d
                       , profile: "myprofile" 
                       , files: [SavedFile {id: 1, name: "yup.jpg", eventId: 1}]}

main = do
  describe "Event json encoding & decoding" do
    it "encodeJson and decodeJson are inverses" do
      n <- (liftEff now :: forall eff. Aff (now :: Now | eff) Date)
      let myEvent = myEventMaker n
          coded = ((readJSONGeneric defaultOptions <<< toJSONGeneric defaultOptions) 
                   myEvent :: Either ForeignError Event)
      shouldEqual coded (Right myEvent)
