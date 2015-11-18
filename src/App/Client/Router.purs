module App.Client.Router where

import Prelude
import DOM (DOM())
import Control.Monad.Eff (Eff())

import OpticUI (Handler(), runHandler)

import Data.Lens (set)
import Data.String (uncons, take, drop)
import Data.Maybe (Maybe(..), maybe)

import App.Client.State

type Nav eff = Route -> Eff eff Unit

-- purescript-routing??

foreign import setHash :: forall eff. String -> Eff (dom :: DOM | eff) Unit
foreign import getHash :: forall eff. Eff (dom :: DOM | eff) String
foreign import hashChanged :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit

nav :: forall eff o. {route :: Route | o} -> Handler (dom :: DOM | eff) {route :: Route | o} -> Nav (dom :: DOM | eff)
nav s h (r@(PhotoboothsPage)) = do 
  setHash "/photobooths"
  runHandler h $ set _route r s
nav s h (r@(EventsPage str)) = do
  setHash $ "/events/" <> str
  runHandler h $ set _route r s

match :: String -> Route
match str = maybe PhotoboothsPage id $ match' str

match' :: String -> Maybe Route
match' str = let t = drop 2 str
              in if (t == "photobooths") 
                    then Just PhotoboothsPage
                    else if (take 6 t == "events") 
                            then Just (EventsPage (drop 7 t))
                            else Nothing
