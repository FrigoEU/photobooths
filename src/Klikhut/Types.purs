module Klikhut.Types where

import OpticUI (UI(), Markup())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import DOM (DOM())
import Network.HTTP.Affjax (AJAX())

type KlikUI eff a = UI eff Markup a a
type AjaxRef eff = (ajax :: AJAX, ref :: REF | eff)
type AjaxRefDom eff = (ajax :: AJAX, ref :: REF, dom :: DOM | eff)
type RefDom eff = (ref :: REF, dom :: DOM | eff)
