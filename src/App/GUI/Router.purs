module App.GUI.Router where

import Prelude

import DOM (DOM())
import Control.Monad.Eff (Eff())
import Network.HTTP.Affjax (AJAX())
import Control.Monad.Eff.Ref (REF())
import Control.Apply ((*>))

import OpticUI (Handler(), runHandler)
import OpticUI.Components.Async (async)

import Data.Lens (set)
import Data.String (drop)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Generic (defaultOptions, readJSONGeneric, toJSONGeneric)

import App.Model.Async (AsyncModel(Busy))
import App.GUI.State
import App.GUI.Load (loadStatistics, loadEvents)
import App.GUI.Types (AjaxRefDom)

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
resolve s (r@(EventsPage cname _)) = do
  eventsAsync <- async $ loadEvents cname
  let modifications = (set (_eventsPage <<< _new <<< _model <<< _computername) cname) <<<
                      (set _events (Busy eventsAsync))
  return (modifications s)
resolve s (r@(StatisticsPage cname _)) = do
  eventsAsync <- async $ loadEvents cname
  statisticsAsync <- async $ loadStatistics cname
  let modifications = (set _events (Busy eventsAsync)) <<< 
                      (set _statistics (Busy statisticsAsync))
  return (modifications s)

match :: String -> Route
match str = either (const PhotoboothsPage) id $ fromUrl (drop 1 str)

{-- fromUrl :: String -> Either Route Unit --}
{-- fromUrl str = do --}
{--   let t = drop 2 str -- #/ --}
{--   if (t `startsWith` "photobooths") then Left PhotoboothsPage --}              
{--                                     else Right unit --}
{--   if (t `startsWith` "events")      then Left (EventsPage (drop (length "events" + 1) t)) --}
{--                                     else Right unit --}
{--   if (t `startsWith` "statistics")  then Left (StatisticsPage (drop (length "statistics" + 1) t)) --} 
{--                                     else Right unit --}

{-- startsWith :: String -> String -> Boolean --}
{-- startsWith str start = take (length start) str == start --}

{-- toUrl :: Route -> String --}
{-- toUrl (EventsPage cname) = "/events/" <> cname --}
{-- toUrl (StatisticsPage cname) = "/statistics/" <> cname --} 
{-- toUrl PhotoboothsPage = "/photobooths" --}

toUrl :: Route -> String
toUrl = toJSONGeneric defaultOptions

fromUrl :: String -> F Route
fromUrl = readJSONGeneric defaultOptions

{-- getBaseUrl :: Route -> String --}
{-- getBaseUrl PhotoboothsPage = "photobooths" --}
{-- getBaseUrl (EventsPage _) = "events" --}
{-- getBaseUrl (StatisticsPage _) = "statistics" --}

{-- matchBase :: forall a. String -> Maybe (MyRoute a) --}
{-- matchBase "photobooths" = Just photoboothsRoute --}
{-- matchBase "events" = Just eventsRoute --}
{-- matchBase "statistics" = Just statisticsRoute --}
{-- matchBase _ = Nothing --}

{-- foreign import getBasePathImpl :: forall a. (a -> Maybe a) -> Maybe a -> String -> Maybe String --}
{-- getBasePath :: String -> Maybe String --}
{-- getBasePath = getBasePathImpl Just Nothing --}

{-- match3 :: forall a. (Serializable a) => String -> Maybe (RouteInstance a) --}
{-- match3 hash = do --}
{--   basepath <- getBasePath hash --}
{--   route <- matchBase basepath --}
{--   either (const Nothing) Just (matchParams route hash) --}

{-- matchParams :: forall a. (Serializable a) => MyRoute a -> String -> Either Error (RouteInstance a) --}
{-- matchParams r@(MyRoute name) hash = do --}
{--   if (hash `startsWith` name) then Right unit else (Left $ error $ hash <> " doesn't start with " <> name) --}
{--   a <- deserialize (drop (length ("#/" <> name <> "?params=")) hash) --}
{--   return $ RouteInstance r a --}

{-- toUrl :: forall a. (Serializable a) => RouteInstance a -> String --}
{-- toUrl (RouteInstance (MyRoute name) a) = "/" <> name <> "?params=" <> serialize a --}

{-- data MyRoute a = MyRoute String --}
{-- data RouteInstance a = RouteInstance (MyRoute a) a --}

{-- photoboothsRoute :: MyRoute Unit --}
{-- photoboothsRoute = MyRoute "photobooths" --}

{-- eventsRoute :: MyRoute String --}
{-- eventsRoute = MyRoute "events" --}

{-- statisticsRoute :: MyRoute String --}
{-- statisticsRoute = MyRoute "statistics" --}

{-- fromUrl :: forall a. (Serializable a) => String -> Either Error (RouteInstance a) --}
{-- fromUrl url = do --}
{--   let t = drop 2 url -- #/ --}
