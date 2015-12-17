module App.GUI.Components.Markup where
  
import Prelude hiding (div)

import OpticUI.Markup.HTML hiding (map, sub)
import OpticUI.Markup
import OpticUI.Core

import Data.Foldable (mconcat, Foldable)

pageTitle :: Markup -> Markup 
pageTitle s = (h1 [classA "page-title"] s)

crudTable :: Markup -> Markup 
crudTable = div [classA "crud-table-wrapper"] <<< table [classA "table crud-table"] <<< tbody []

tableHeader :: forall f. (Functor f, Foldable f) => Array Prop -> f String -> Markup
tableHeader ps hs = tr ps $ mconcat (th [] <<< text <$> hs) 

emptyTd :: Markup 
emptyTd = td [] (text "")

tdUIs :: forall eff b c. Array (UI eff Markup b c) -> UI eff Markup b c
tdUIs uis = mconcat $ withView (td []) <$> uis

rowUI :: forall eff b c. Array (UI eff Markup b c) -> UI eff Markup b c
rowUI = withView (tr []) <<< tdUIs

