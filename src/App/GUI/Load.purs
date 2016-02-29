module App.GUI.Load where

import Prelude

import Network.HTTP.Affjax (AJAX())
import Control.Monad.Aff (Aff())

import Data.Maybe (Maybe(Nothing))

import App.Model.Event
import App.Model.Statistic
import App.Model.Async
import App.GUI.State
import App.GUI.Types
import App.Endpoint

loadEvents :: forall eff. String -> Aff (ajax :: AJAX | eff) (Array (EventWithState (ajax :: AJAX | eff)))
loadEvents s = execEndpoint getEvents s unit >>= sortEvents >>> \es -> return $ map {model: _, state: {savingFile: Initial, file: Nothing}} es

loadStatistics :: forall eff. String -> Aff (ajax :: AJAX | eff) AllStatistics
loadStatistics s = execEndpoint getStatistics s unit

