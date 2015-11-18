module App.Client.Components.Select where

import Prelude

import OpticUI (with, ui, runHandler, UI())
import OpticUI.Markup (Prop(), Markup(), text)
import OpticUI.Markup.HTML as H

import Data.Maybe (maybe, Maybe(..))
import Data.Foldable(find, foldMap)

select :: forall eff a. (Show a) => Array a -> Array Prop -> UI eff Markup a a
select os ps = with $ \s h ->
  let handler _ (Just v) = maybe (return unit) (\o -> runHandler h o) (find (\o -> show o == v) os) -- If for some reason we can't find the selected option, don't change the selection
      handler _ Nothing = return unit
      props = ps ++ [H.onInput handler, H.valueA $ show s]
   in ui $ H.select props $ foldMap (\o -> H.option [H.valueA $ show o] $ text $ show o) os
