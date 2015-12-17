module App.GUI.Components.CrudButtons where
  
import Prelude

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Core
import OpticUI.Components.Async (onResult)

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (REF())

import Data.Maybe (Maybe(..))

import App.GUI.Types
import App.GUI.Views.Crud
import App.Model.Async

editButton :: forall eff a b c d. (CrudCommand d -> Eff eff Unit) -> Int -> Maybe {index :: Int, saving :: AsyncModel eff a | b} -> AppUI eff c
editButton handle i editing = c editing
  where 
    c Nothing                          = ui $ H.button [H.classA "btn btn-action", H.onClick (const $ handle $ StartEdit i)] $ H.text "Edit!"
    c (Just {index: selI}) | selI /= i = ui $ H.div [] $ H.text ""
    c (Just {saving: Busy _})          = (ui $ H.button [H.classA "btn btn-warning"] $ H.text "Saving")
    c (Just {saving: Errored err})     = (ui $ H.button [H.classA "btn btn-danger", H.onClick \_ -> handle (SaveEdit)] $ H.text "Save failed, try again")
                                      <> (ui $ H.div [H.classA "alert alert-danger"] $ H.text $ message err)
    c (Just {saving: _})               = (ui $ H.button [H.classA "btn btn-success", H.onClick \_ -> handle (SaveEdit)] $ H.text "Save edit")
                                      <> (ui $ H.button [H.classA "btn btn-warning", H.onClick \_ -> handle (CancelEdit)] $ H.text "Cancel edit")
                                      
newButton :: forall eff a. (CrudCommand a -> Eff (ref :: REF | eff) Unit)
                              -> AppUI (ref :: REF | eff) (AsyncModel (ref :: REF | eff) a)
newButton handle = with c
  where 
    c (Busy _)      h = (ui $ H.button [H.classA "btn btn-warning"] $ H.text "Saving...")
                     <> (_Busy $ onResult (handle <<<  NewSaved) (handle <<< NewSaveFailed))
    c (Errored err) h = (ui $ H.button [H.classA "btn btn-danger", H.onClick \_ -> handle SaveNew] (H.text "Saving Failed, Try Again?"))
                     <> (ui $ H.div [H.classA "alert alert-danger"] $ H.text (message err))
    c _             h =  ui $ H.button [H.classA "btn btn-action", H.onClick \_ -> handle SaveNew] $ H.text "Save!"

