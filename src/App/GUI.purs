module App.GUI where

import Prelude (Unit, ($), bind, (<$>), return)
import Control.Monad.Eff (Eff())

import OpticUI(animate, with)

import Data.Lens (view, set)

import App.GUI.Views.PhotoboothsPage (photoboothsPage)
import App.GUI.Views.EventsPage (eventsPage)
import App.GUI.Views.StatisticsPage (statisticsPage)
import App.GUI.Router (match, resolve, hashChanged, nav, getHash)
import App.GUI.Types (ANDRT)
import App.GUI.State (Route(StatisticsPage, EventsPage, PhotoboothsPage), _statisticsPage, _eventsPage, _pbPage, _route, initialState)

------ MAIN -----------

main :: forall eff. Eff (ANDRT eff) Unit
main = do
  let initRoute = PhotoboothsPage
  ini <- initialState initRoute
  matchedRoute <- match <$> getHash
  newS <- resolve ini matchedRoute
  let newSWithRoute = set _route matchedRoute newS
  driver <- animate newSWithRoute $ with \s h ->
    let nav' = nav s h
     in case view _route s of
            PhotoboothsPage -> _pbPage (photoboothsPage nav')
            (EventsPage cn alias page) -> _eventsPage $ eventsPage cn alias page nav'
            (StatisticsPage _ alias) -> _statisticsPage $ statisticsPage alias nav'
  hashChanged (\str -> driver (\s -> do let newRoute = match str
                                        newSt <- resolve s newRoute
                                        return $ set _route newRoute newSt))
