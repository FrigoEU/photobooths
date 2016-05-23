module App.GUI.Router where

import App.GUI.Load (loadStatistics, loadEvents, loadEventsWithState)
import App.GUI.State (State, Route(PhotoboothsPage, StatisticsPage, EventsPage), _statistics, _statisticsPage, _events, _collection, _eventsPage, _computername, _model, _new, _route)
import App.GUI.Types (AjaxRefDom)
import App.Model.Async (AsyncModel(Busy))
import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Generic (defaultOptions, readJSONGeneric, toJSONGeneric)
import Data.Lens (set)
import Data.String (drop)
import Network.HTTP.Affjax (AJAX)
import OpticUI (Handler, runHandler)
import OpticUI.Components.Async (async)
import Prelude (Unit, id, const, ($), return, (<<<), bind, (>>>), (>>=))

type Nav eff = Route -> Eff eff Unit

foreign import setHash :: forall eff. String -> Eff (dom :: DOM | eff) Unit
foreign import getHash :: forall eff. Eff (dom :: DOM | eff) String
foreign import hashChanged :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit

nav :: forall eff.  State (AjaxRefDom eff) -> 
                    Handler (AjaxRefDom eff) (State (AjaxRefDom eff)) -> 
                    Nav (AjaxRefDom eff)
nav s h r = setHash (toUrl r) *> resolve s r >>= set _route r >>> runHandler h
{-- nav s h r = do --}
{--   newState <- resolve s r --}
{--   let stateWithRoute = set _route r newState --} 
{--   setHash (toUrl r) --} 
{--   runHandler h stateWithRoute --}

resolve :: forall eff. State (ajax :: AJAX, ref :: REF | eff) -> Route -> 
                    Eff (ajax :: AJAX, ref :: REF | eff) (State (ajax :: AJAX, ref :: REF | eff))
resolve s PhotoboothsPage = return s
resolve s (r@(EventsPage cname _ page)) = do
  eventsAsync <- async $ loadEventsWithState cname page
  let modifications = (set (_eventsPage <<< _new <<< _model <<< _computername) cname) <<<
                      (set (_eventsPage <<< _collection) (Busy eventsAsync))
  return (modifications s)
resolve s (r@(StatisticsPage cname _)) = do
  eventsAsync <- async $ loadEvents cname
  statisticsAsync <- async $ loadStatistics cname
  let modifications = (set (_statisticsPage <<< _events) (Busy eventsAsync)) <<< 
                      (set (_statisticsPage <<< _statistics) (Busy statisticsAsync))
  return (modifications s)

match :: String -> Route
match str = either (const PhotoboothsPage) id $ fromUrl (drop 1 str)

toUrl :: Route -> String
toUrl = toJSONGeneric defaultOptions

fromUrl :: String -> F Route
fromUrl = readJSONGeneric defaultOptions
