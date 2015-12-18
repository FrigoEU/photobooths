module App.GUI.Components.DateTimeField where

import Prelude
import DOM(DOM())

import OpticUI (with, ui, runHandler, UI())
import OpticUI.Markup (Prop(), Markup())
import OpticUI.Markup.HTML as H

import Data.Maybe
import Data.String (take)
import Data.Date (Date(), now, Now())

import App.Model.Date

dateTimeField :: forall eff. Array Prop -> UI (now :: Now, dom :: DOM | eff) Markup Date Date
dateTimeField ps = with $ \s h -> 
  let handler _ v = maybe (now >>= \d -> runHandler h d) (\d -> runHandler h d) $ (fromLocalDatetime (maybe "" id v))
      props = [H.typeA "datetime-local", H.onInput handler, H.valueA $ take 16 $ toLocalDatetime s]
   in ui $ H.input_ (ps ++ props) 
