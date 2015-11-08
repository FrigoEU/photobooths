module ClientState where

import Prelude
import Data.Lens (lens, Lens(), LensP())
import Klikhut.Async

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import DOM (DOM())
import Network.HTTP.Affjax (AJAX())
import Klikhut.Model.Photobooth
import Data.Maybe(Maybe(..))

-------------------------------------------

initialState :: State
initialState = { route: PhotoboothsPage
               , photobooths: Initial
               , photoboothsPage: 
                 { new: 
                   { model: 
                     { computername: "", alias: "", defaultprofile: ""}
                       , state: Initial}
                 , editing: Nothing }}

type State = forall eff. 
             { route :: Route
             , photobooths :: AsyncModel eff (Array Photobooth)
             , photoboothsPage :: 
               { new :: 
                 { model :: 
                   { computername :: String, alias :: String, defaultprofile :: String}
                   , state :: AsyncModel eff Unit}
               , editing :: Maybe {index:: Int, previous:: Photobooth, saving:: AsyncModel eff Unit}}}
 
------ ROUTER ----------------------------

data Route = PhotoboothsPage
           | EventsPage String

------ LENSES --------------------------

_collection :: forall a b o. Lens {collection :: a | o} {collection :: b | o} a b
_collection = lens _.collection (_ {collection = _})

_collectionEditing = lens (\{collection: a, editing: b} -> {collection: a, editing: b})
                           (\old {collection: a, editing: b} -> old {collection = a, editing = b})

_new :: forall a b o. Lens {new :: a | o} {new :: b | o} a b
_new = lens _.new (_ {new = _})

_model :: forall a b o. Lens {model :: a | o} {model :: b | o} a b
_model = lens _.model (_ {model = _})

_state :: forall a b o. Lens {state :: a | o} {state :: b | o} a b
_state = lens _.state (_ {state = _})

_editing :: forall a b o. Lens {editing :: a | o} {editing :: b | o} a b
_editing = lens _.editing (_ {editing = _})

_index :: forall a b o. Lens {index :: a | o} {index :: b | o} a b
_index = lens _.index (_ {index = _})

_previous :: forall a b o. Lens {previous :: a | o} {previous :: b | o} a b
_previous = lens _.previous (_ {previous = _})

_saving :: forall a b o. Lens {saving :: a | o} {saving :: b | o} a b
_saving = lens _.saving (_ {saving = _})

_route :: forall a b o. Lens {route :: a | o} {route :: b | o} a b
_route = lens _.route (_ {route = _})

_pbPage :: forall a b c d o. LensP {photobooths :: a, photoboothsPage :: {new :: b, editing :: c}, route :: d | o}
                                {collection :: a, new :: b, editing :: c, route :: d}
_pbPage = lens (\obj -> {collection: obj.photobooths
                       , new: obj.photoboothsPage.new
                       , editing: obj.photoboothsPage.editing
                       , route: obj.route})
               (\old obj ->
                   old {photobooths = obj.collection
                       , photoboothsPage = {new: obj.new, editing: obj.editing}
                       , route = obj.route})




