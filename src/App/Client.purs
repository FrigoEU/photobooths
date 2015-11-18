module App.Client where

import Prelude
import Control.Monad.Eff (Eff())

import OpticUI(animate, with)

import Data.Lens (view, set)

import App.Client.Views.PhotoboothsPage
import App.Client.Views.EventsPage
import App.Client.Router
import App.Client.Types
import App.Client.State

------ MAIN -----------

main :: forall eff. Eff (ANDRT eff) Unit
main = do
  initRoute <- match <$> getHash
  ini <- initialState initRoute
  driver <- animate ini $ with \s h ->
    let nav' = nav s h
     in case view _route s of
            PhotoboothsPage -> _pbPage (photoboothsPage nav')
            (EventsPage computername) -> _eventsPage $ eventsPage computername
  hashChanged (\str -> driver (\s -> set _route (match str) s))
