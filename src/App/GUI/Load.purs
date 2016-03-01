module App.GUI.Load where

import Prelude

import Network.HTTP.Affjax (AJAX())
import Control.Monad.Aff (Aff())

import Data.Maybe (Maybe(Nothing))
import Data.Tuple

import App.Model.Event
import App.Model.Statistic
import App.Model.Async
import App.GUI.State
import App.GUI.Types
import App.Endpoint

loadEvents :: forall eff. String -> Aff (ajax :: AJAX | eff) (Array Event)
loadEvents s = execEndpoint getEvents s unit

loadEventsWithState :: forall eff. String -> Int -> Aff (ajax :: AJAX | eff) (Array (EventWithState (ajax :: AJAX | eff)))
loadEventsWithState s page = map wrapEvent <$> sortEvents <$> execEndpoint getEventsPaged (Tuple s page) unit

wrapEvent = {model: _, state: {savingFile: Initial, file: Nothing}}

loadStatistics :: forall eff. String -> Aff (ajax :: AJAX | eff) AllStatistics
loadStatistics s = execEndpoint getStatistics s unit

