module Klikhut.Client.EventsPage (eventsPage) where

import Prelude

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Components (textField)
import OpticUI (with, withView, traversal, ui, text, UI(), Markup(), runHandler, inline, foreach)
import OpticUI.Components.Async (onResult, async)

import Klikhut.Model.Event
import Klikhut.Endpoint
import Klikhut.Types
import Klikhut.Async
import ClientState

{-- eventsPage :: forall eff. KlikUI (AjaxRefDom eff) {} --}
eventsPage cn = with \s h -> ui $ H.div [] $ H.text cn

-- datetime-local
