module App.Client.Components.Exec where

import Prelude

import OpticUI (inline, UI())
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import DOM.Timer(Timer(), timeout)
import Data.Monoid (mempty, Monoid)

exec :: forall eff v s t. (Monoid v) => Eff (timer :: Timer | eff) Unit -> UI (timer :: Timer | eff) v s t
exec eff = inline (timeout 0 eff *> return mempty)
