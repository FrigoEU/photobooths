module App.GUI.Router where

import Prelude
import DOM (DOM())
import Control.Monad.Eff (Eff())
import Network.HTTP.Affjax (AJAX())
import Control.Monad.Eff.Ref (REF())

import OpticUI (Handler(), runHandler)
import OpticUI.Components.Async (async)

import Data.Lens (set)
import Data.String (take, drop)
import Data.Tuple (Tuple(..))
import Data.Either

import App.Model.Async
import App.GUI.State
import App.GUI.Load
import App.GUI.Types

type Nav eff = Route -> Eff eff Unit

-- purescript-routing??

foreign import setHash :: forall eff. String -> Eff (dom :: DOM | eff) Unit
foreign import getHash :: forall eff. Eff (dom :: DOM | eff) String
foreign import hashChanged :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit

nav :: forall eff. 
       State (AjaxRefDom eff) 
       -> Handler (AjaxRefDom eff) (State (AjaxRefDom eff)) -> Nav (AjaxRefDom eff)
nav s h r = nav_ s r >>= \(Tuple hash newS) -> do 
                            setHash hash 
                            runHandler h newS

nav_ :: forall eff. State (ajax :: AJAX, ref :: REF | eff) -> Route -> 
                    Eff (ajax :: AJAX, ref :: REF | eff) (Tuple String (State (ajax :: AJAX, ref :: REF | eff)))
nav_ s (r@(PhotoboothsPage)) = return $ Tuple "/photobooths" (set _route r s) 
nav_ s (r@(EventsPage cname)) = do
  eventsAsync <- async $ loadEvents cname
  let modifications = (set _route r) <<<
                      (set (_eventsPage <<< _new <<< _model <<< _computername) cname) <<<
                      (set _events (Busy eventsAsync))
  return $ Tuple ("/events/" <> cname) (modifications s)
nav_ s (r@(StatisticsPage cname)) = do
  eventsAsync <- async $ loadEvents cname
  statisticsAsync <- async $ loadStatistics cname
  let modifications = (set _route r) <<< 
                      (set _events (Busy eventsAsync)) <<< 
                      (set _statistics (Busy statisticsAsync))
  return $ Tuple ("/statistics/" <> cname) (modifications s)

match :: String -> Route
match str = either id (const PhotoboothsPage) $ match' str

match' :: String -> Either Route Unit
match' str = do
  let t = drop 2 str -- #/
  if (t         == "photobooths") then Left PhotoboothsPage              else Right unit
  if (take 6  t == "events"     ) then Left (EventsPage (drop 7 t))      else Right unit
  if (take 10 t == "statistics" ) then Left (StatisticsPage (drop 11 t)) else Right unit
