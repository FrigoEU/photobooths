module Client where

import Prelude

import OpticUI(animate, with)
import Data.Lens (view)

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import DOM (DOM())
import Network.HTTP.Affjax (AJAX())

import Klikhut.Client.PhotoboothsPage
import Klikhut.Client.EventsPage
import ClientState

------ MAIN -----------

main :: forall eff. Eff (ajax :: AJAX, ref :: REF, dom :: DOM | eff) Unit
main = animate initialState $ with \s h ->
       case view _route s of
            PhotoboothsPage -> _pbPage photoboothsPage
            (EventsPage computername) -> eventsPage computername


