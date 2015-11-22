module App.GUI.Types where

import OpticUI (UI(), Markup())

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())

import DOM (DOM())
import DOM.Timer(Timer())
import Network.HTTP.Affjax (AJAX())

import Data.Date(Now())

type AppUI eff a = UI eff Markup a a
type AjaxRef eff = (ajax :: AJAX, ref :: REF | eff)
type AjaxRefDom eff = (ajax :: AJAX, ref :: REF, dom :: DOM | eff)
type AjaxRefDomTimer eff = (ajax :: AJAX, ref :: REF, dom :: DOM, timer :: Timer | eff)
type RefDom eff = (ref :: REF, dom :: DOM | eff)
type RefDomTimer eff = (ref :: REF, dom :: DOM, timer :: Timer | eff)
type TimerAjaxRef eff = (ref :: REF, ajax :: AJAX, timer :: Timer | eff)
type AjaxNowRef eff = (ajax :: AJAX, now :: Now, ref :: REF | eff)
type ANDR eff = (ajax :: AJAX, now :: Now, ref :: REF, dom :: DOM | eff)
type ANDRT eff = (timer :: Timer, ajax :: AJAX, now :: Now, ref :: REF, dom :: DOM | eff)
type NowDomRefTimer eff = (timer :: Timer, now :: Now, ref :: REF, dom :: DOM | eff)
type NowDomRef eff = (now :: Now, ref :: REF, dom :: DOM | eff)
