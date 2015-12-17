module App.GUI where

import Prelude
import Control.Monad.Eff (Eff())

import OpticUI(animate, with)

import Data.Lens (view)
import Data.Tuple (Tuple(..))

import App.GUI.Views.PhotoboothsPage
import App.GUI.Views.EventsPage
import App.GUI.Views.StatisticsPage
import App.GUI.Router
import App.GUI.Types
import App.GUI.State

------ MAIN -----------

main :: forall eff. Eff (ANDRT eff) Unit
main = do
  let initRoute = PhotoboothsPage
  ini <- initialState initRoute
  matchedRoute <- match <$> getHash
  (Tuple _ newS) <- nav_ ini matchedRoute
  driver <- animate newS $ with \s h ->
    let nav' = nav s h
     in case view _route s of
            PhotoboothsPage -> _pbPage (photoboothsPage nav')
            (EventsPage cname) -> _eventsPage $ eventsPage cname
            (StatisticsPage cname) -> _statisticsPage $ statisticsPage cname
  hashChanged (\str -> driver (\s -> nav_ s (match str) >>= \(Tuple _ newS) -> return newS))
