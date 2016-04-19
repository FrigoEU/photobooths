module App.GUI.Views.LoginPage where

import App.Endpoint (login)
import App.GUI.Components.Exec (exec)
import App.GUI.Router (Nav)
import App.GUI.State (Route(PhotoboothsPage), _loggingIn, _password, _username)
import App.GUI.Types (AppUI)
import App.Model.Async (_Done, AsyncModel(Done, Errored, Busy), _Busy, _Errored)
import App.Model.Session (Session)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Timer (Timer)
import Data.Foldable (mconcat)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Endpoint.Client (execEndpoint)
import Network.HTTP.Affjax (AJAX)
import OpticUI (runHandler, withView, ui, text, textField, with)
import OpticUI.Components.Async (onResult, async)
import OpticUI.Markup.HTML (classA, div, onClick, button, typeA, label)
import Prelude (($), (<<<), (<>), unit, (>>=))

type Lpeff eff = (dom :: DOM, ajax :: AJAX, ref :: REF, timer :: Timer| eff)

loginPage :: forall eff. Nav (Lpeff eff)
                    -> AppUI (Lpeff eff) { session :: Maybe Session
                                         , username :: String
                                         , password :: String
                                         , loggingIn :: AsyncModel (Lpeff eff) Session }
loginPage nav = with c
  where 
    c s h = 
      let asyncLogin = async (execEndpoint login unit (Tuple s.username s.password)) 
                         >>= (\a -> runHandler h (s {loggingIn = Busy a}))
       in withView (div [classA "login-page"] <<< div [classA "login-container"]) $ mconcat [ 
            withView (label []) $ (ui $ text "username") 
                                  <> (_username $ textField [classA "form-control"]),
            withView (label []) $ (ui $ text "password") 
                                  <> (_password $ textField [classA "form-control", typeA "password"]),
            ui $ button [classA "btn btn-primary", onClick (\_ -> asyncLogin) ] $ text "Login",
            _loggingIn <<< _Errored $ with 
              (\err _ -> ui $ div [classA "alert alert-danger"] $ text $ message err),
            _loggingIn <<< _Busy $ 
              onResult (\sess -> runHandler h (s {loggingIn = Done sess, session = Just sess}))
                       (\err -> runHandler h (s {loggingIn = Errored err})),
            _loggingIn <<< _Done $ exec (nav PhotoboothsPage)
        ]
