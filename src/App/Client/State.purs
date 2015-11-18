module App.Client.State where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())

import DOM (DOM())
import DOM.File.Types (File())
import Network.HTTP.Affjax (AJAX())

import Data.Lens (lens, Lens(), LensP())
import Data.Maybe(Maybe(..))
import Data.Date (Date(), now, Now())

import App.Model.Async
import App.Model.Photobooth as PB
import App.Model.Event as E
import App.Model.SavedImage

-------------------------------------------

initialState :: Route -> forall eff. Eff (now :: Now | eff) (State (now :: Now | eff)) 
initialState initRoute = do
  dateNow <- now
  return { route: initRoute
  , photobooths: Initial
  , events: Initial
  , eventsPage: 
    { new: 
      { model: 
        { id: Nothing, computername: "", name: "", datefrom: dateNow, dateuntil: dateNow, profile: "", images: []}
      , state: Initial}
    , editing: Nothing}
  , photoboothsPage: 
    { new: 
      { model: 
        { id: Nothing, computername: "", alias: "", defaultprofile: ""}
          , state: Initial}
    , editing: Nothing }}

type State eff = { route :: Route
                 , photobooths :: AsyncModel eff (Array PB.Photobooth)
                 , events :: AsyncModel eff (Array (EventWithState eff))
                 , eventsPage :: 
                   { new :: 
                     { model :: { id :: Maybe Int, computername :: String, name :: String, datefrom :: Date, dateuntil :: Date, profile :: String, images :: Array SavedImage}
                     , state :: AsyncModel eff (EventWithState eff)}
                   , editing :: Maybe {index:: Int, previous:: EventWithState eff, saving:: AsyncModel eff (EventWithState eff)}}
                 , photoboothsPage :: 
                   { new :: 
                     { model :: 
                       { id:: Maybe Int, computername :: String, alias :: String, defaultprofile :: String}
                       , state :: AsyncModel eff PB.Photobooth}
                   , editing :: Maybe {index:: Int, previous:: PB.Photobooth, saving:: AsyncModel eff PB.Photobooth}}
                 }

type EventWithState eff = {model :: E.Event, state :: {savingImage :: AsyncModel eff SavedImage, image :: Maybe File}}

------ ROUTES --------------------------

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

_eventsPage :: forall a b c d o. LensP {events :: a, eventsPage :: {new :: b, editing :: c}, route :: d | o}
                                {collection :: a, new :: b, editing :: c, route :: d}
_eventsPage = lens (\obj -> {collection: obj.events
                       , new: obj.eventsPage.new
                       , editing: obj.eventsPage.editing
                       , route: obj.route})
               (\old obj ->
                   old {events = obj.collection
                       , eventsPage = {new: obj.new, editing: obj.editing}
                       , route = obj.route})

_id :: forall a b o. Lens {id :: a | o} {id :: b | o} a b
_id = lens _.id (_ {id = _})

_name :: forall a b o. Lens {name :: a | o} {name :: b | o} a b
_name = lens _.name (_ {name = _})

_datefrom :: forall a b o. Lens {datefrom :: a | o} {datefrom :: b | o} a b
_datefrom = lens _.datefrom (_ {datefrom = _})

_dateuntil :: forall a b o. Lens {dateuntil :: a | o} {dateuntil :: b | o} a b
_dateuntil = lens _.dateuntil (_ {dateuntil = _})

_profile :: forall a b o. Lens {profile :: a | o} {profile :: b | o} a b
_profile = lens _.profile (_ {profile = _})

_computername :: forall a b o. Lens {computername :: a | o} {computername :: b | o} a b
_computername = lens _.computername (_ {computername = _})

_alias :: forall a b o. Lens {alias :: a | o} {alias :: b | o} a b
_alias = lens _.alias (_ {alias = _})

_defaultprofile :: forall a b o. Lens {defaultprofile :: a | o} {defaultprofile :: b | o} a b
_defaultprofile = lens _.defaultprofile (_ {defaultprofile = _})

_images :: forall a b o. Lens {images :: a | o} {images :: b | o} a b
_images = lens _.images (_ {images = _})

_eventId :: forall a b o. Lens {eventId :: a | o} {eventId :: b | o} a b
_eventId = lens _.eventId (_ {eventId = _})

_image :: forall a b o. Lens {image :: a | o} {image :: b | o} a b
_image = lens _.image (_ {image = _})

_savingImage :: forall a b o. Lens {savingImage :: a | o} {savingImage :: b | o} a b
_savingImage = lens _.savingImage (_ {savingImage = _})

