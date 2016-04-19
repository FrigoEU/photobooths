module App.GUI where

import App.GUI.Router (nav, match, resolve, hashChanged, getHash)
import App.GUI.State (Route(StatisticsPage, EventsPage, PhotoboothsPage, LoginPage), _route, _statisticsPage, _eventsPage, _pbPage, _loginPage, initialState)
import App.GUI.Types (ANDRT)
import App.GUI.Views.EventsPage (eventsPage)
import App.GUI.Views.LoginPage (loginPage)
import App.GUI.Views.PhotoboothsPage (photoboothsPage)
import App.GUI.Views.StatisticsPage (statisticsPage)
import Control.Monad.Eff (Eff)
import Data.Lens (view, set)
import Data.Maybe (isNothing)
import OpticUI (animate, with)
import Prelude (Unit, ($), bind, (<$>), return)

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
     in if isNothing s.session 
           then _loginPage (loginPage nav')
           else case view _route s of
                     LoginPage -> _loginPage (loginPage nav')
                     PhotoboothsPage -> _pbPage (photoboothsPage nav')
                     (EventsPage cn alias page) -> _eventsPage $ eventsPage cn alias page nav'
                     (StatisticsPage _ alias) -> _statisticsPage $ statisticsPage alias nav'
  hashChanged (\str -> driver (\s -> do let newRoute = match str
                                        newSt <- resolve s newRoute
                                        return $ set _route newRoute newSt))
