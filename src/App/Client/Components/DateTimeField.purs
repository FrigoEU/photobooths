module App.Client.Components.DateTimeField where

import Prelude
import DOM(DOM())
import Control.Monad.Eff (Eff())

import OpticUI (with, ui, runHandler, UI())
import OpticUI.Markup (Event(), Prop(), handle, Markup())
import OpticUI.Markup.HTML as H

import Data.Maybe
import Data.String (take)
import Data.Date (Date(), fromString, now, Now())
import Data.Foreign.Class (IsForeign)

import App.Client.Types
import App.Model.Date

dateTimeField :: forall eff. Array Prop -> UI (now :: Now, dom :: DOM | eff) Markup Date Date
dateTimeField ps = with $ \s h -> 
  let handler _ v = maybe (now >>= \d -> runHandler h d) (\d -> runHandler h d) $ (fromString (maybe "" id v))
      props = [H.typeA "datetime-local", H.onInput handler, H.valueA $ take 16 $ iso8601 s]
   in ui $ H.input_ (ps ++ props) 
