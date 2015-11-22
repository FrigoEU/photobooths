module App.GUI.Components.Select where

import Prelude

import OpticUI (with, ui, runHandler, UI())
import OpticUI.Markup (Prop(), Markup(), text, prop)
import OpticUI.Markup.HTML as H

import Data.Maybe (maybe, Maybe(..))
import Data.Foldable(find, foldMap)

select :: forall eff a. Array a -> (a -> String) -> Array Prop -> UI eff Markup a a
select os toStr ps = with $ \s h ->
  let handler _ (Just v) = maybe (return unit) (\o -> runHandler h o) (find (\o -> toStr o == v) os) -- If for some reason we can't find the selected option, don't change the selection
      handler _ Nothing = return unit
      sToStr = toStr s
      options = case find (\o -> toStr o == sToStr) os of
                     Nothing -> map toStr os <> [sToStr]
                     Just _ -> map toStr os
      props = ps ++ [H.onInput handler]
   in ui $ H.select props $ foldMap (\o -> H.option [H.valueA o, selected (o == sToStr)] $ text o) options

selected :: Boolean -> Prop
selected = prop "selected"
