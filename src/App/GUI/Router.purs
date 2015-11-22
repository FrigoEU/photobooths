module App.GUI.Router where

import Prelude
import DOM (DOM())
import Control.Monad.Eff (Eff())
import Network.HTTP.Affjax (AJAX())
import Control.Monad.Eff.Ref (REF())

import OpticUI (Handler(), runHandler)
import OpticUI.Components.Async (onResult, async)

import Data.Lens (set)
import Data.String (uncons, take, drop)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import App.Model.Async
import App.GUI.State
import App.GUI.Load
import App.GUI.Types

type Nav eff = Route -> Eff eff Unit

-- purescript-routing??

foreign import setHash :: forall eff. String -> Eff (dom :: DOM | eff) Unit
foreign import getHash :: forall eff. Eff (dom :: DOM | eff) String
foreign import hashChanged :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit

nav :: forall eff o. 
       State (AjaxRefDom eff) 
       -> Handler (AjaxRefDom eff) (State (AjaxRefDom eff)) -> Nav (AjaxRefDom eff)
nav s h r = nav_ s r >>= \(Tuple hash newS) -> do 
                   setHash hash 
                   runHandler h newS

nav_ :: forall eff a. State (ajax :: AJAX, ref :: REF | eff) -> Route -> Eff (ajax :: AJAX, ref :: REF | eff) (Tuple String (State (ajax :: AJAX, ref :: REF | eff)))
nav_ s (r@(PhotoboothsPage)) = return $ Tuple "/photobooths" (set _route r s) 
nav_ s (r@(EventsPage cname)) = do
  eventsAsync <- async $ loadEvents cname
  let modifications = (set _route r) <<<
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
match str = maybe PhotoboothsPage id $ match' str

match' :: String -> Maybe Route
match' str = let t = drop 2 str
              in if (t == "photobooths") 
                    then Just PhotoboothsPage
                    else if (take 6 t == "events") 
                            then Just (EventsPage (drop 7 t))
                            else if(take 10 t == "statistics")
                                  then Just (StatisticsPage (drop 11 t))
                                  else Nothing
