module App.GUI.Views.Profiles where

import Prelude

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI (with, withView, traversal, ui, text, UI(), Markup(), runHandler, foreach)
import OpticUI.Components.Async (onResult, async)

import Control.Monad.Eff.Exception (message, Error())
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff())

import Data.Monoid (mempty)
import Data.StrMap (StrMap())

import Prelude
import Network.HTTP.Affjax (AJAX())

import App.Model.Profile
import App.Model.Async
import App.Model.StrMap
import App.GUI.Types
import App.GUI.State
import App.GUI.Components.Exec
import App.Endpoint

loadProfiles :: forall eff. AppUI (TimerAjaxRef eff) (AsyncModel (TimerAjaxRef eff) Profiles)
loadProfiles = with c
  where
    c Initial h = exec $ async allProfiles >>= (\a -> runHandler h (Busy a))
    c (Busy _) h = (ui $ H.div [] $ H.text "Loading profiles")
                <> (_Busy $ onResult (runHandler h <<< Done) (runHandler h <<< Errored))
    c (Errored err) _ = ui $ H.div [H.classA "alert alert-danger"] $ H.text ("Failed to load profiles: " <> message err)
    c _ _ = mempty



allProfiles :: forall eff. Aff (ajax :: AJAX | eff) (StrMap (Array String))
allProfiles = fromArray <$> execEndpoint getProfiles unit unit 
