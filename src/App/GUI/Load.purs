module App.GUI.Load where

import Prelude (unit, (<$>), map)

import Network.HTTP.Affjax (AJAX())
import Control.Monad.Aff (Aff())

import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(Tuple))

import App.Model.Event (Event, sortEvents)
import App.Model.Statistic (AllStatistics)
import App.Model.Async (AsyncModel(Initial))
import App.GUI.State (EventWithState)
import App.Endpoint (getStatistics, execEndpoint, getEventsPaged, getEvents)

loadEvents :: forall eff. String -> Aff (ajax :: AJAX | eff) (Array Event)
loadEvents s = execEndpoint getEvents s unit

loadEventsWithState :: forall eff. String -> Int -> Aff (ajax :: AJAX | eff) (Array (EventWithState (ajax :: AJAX | eff)))
loadEventsWithState s page = map wrapEvent <$> sortEvents <$> execEndpoint getEventsPaged (Tuple s page) unit

wrapEvent :: forall t1 t2 t3 t4. t1 -> { model :: t1 , state :: { savingFile :: AsyncModel t3 t2 , file :: Maybe t4 } }
wrapEvent = {model: _, state: {savingFile: Initial, file: Nothing}}

loadStatistics :: forall eff. String -> Aff (ajax :: AJAX | eff) AllStatistics
loadStatistics s = execEndpoint getStatistics s unit

