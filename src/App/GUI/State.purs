module App.GUI.State where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())

import DOM (DOM())
import DOM.File.Types (File())
import Network.HTTP.Affjax (AJAX())

import Data.Lens (lens, Lens(), LensP())
import Data.Maybe(Maybe(..))
import Data.Date (Date(), now, Now())
import Data.Generic (class Generic)

import App.Model.Async
import App.Model.Photobooth as PB
import App.Model.Event as E
import App.Model.Profile
import App.Model.SavedFile
import App.Model.Statistic

-------------------------------------------

initialState :: Route -> forall eff. Eff (now :: Now | eff) (State (now :: Now | eff)) 
initialState initRoute = do
  dateNow <- now
  return { route: initRoute
  , photobooths: Initial
  , events: Initial
  , profiles: Initial
  , statistics: Initial
  , eventsPage: 
    { new: 
      { model: 
        { id: Nothing, computername: "", name: "", datefrom: dateNow, dateuntil: dateNow, profile: "", files: []}
      , state: Initial}
    , editing: Nothing
    , deleting: Nothing}
  , photoboothsPage: 
    { new: 
      { model: 
        { id: Nothing, computername: "", alias: "", defaultprofile: ""}
          , state: Initial}
    , editing: Nothing 
    , deleting: Nothing}}

type State eff = { route :: Route
                 , photobooths :: AsyncModel eff (Array PB.Photobooth)
                 , events :: AsyncModel eff (Array (EventWithState eff))
                 , profiles :: AsyncModel eff Profiles
                 , statistics :: AsyncModel eff AllStatistics
                 , eventsPage :: 
                   { new :: 
                     { model :: { id :: Maybe Int, computername :: String, name :: String, datefrom :: Date, dateuntil :: Date, profile :: String, files :: Array SavedFile}
                     , state :: AsyncModel eff (EventWithState eff)}
                   , editing :: Maybe {index:: Int, previous:: EventWithState eff, saving:: AsyncModel eff (EventWithState eff)}
                   , deleting :: Maybe {index :: Int, saving :: AsyncModel eff Unit}}
                 , photoboothsPage :: 
                   { new :: 
                     { model :: 
                       { id:: Maybe Int, computername :: String, alias :: String, defaultprofile :: String}
                       , state :: AsyncModel eff PB.Photobooth}
                   , editing :: Maybe {index:: Int, previous:: PB.Photobooth, saving:: AsyncModel eff PB.Photobooth}
                   , deleting :: Maybe {index :: Int, saving :: AsyncModel eff Unit}}
                 }

type EventWithState eff = {model :: E.Event, state :: {savingFile :: AsyncModel eff SavedFile, file :: Maybe File}}

------ ROUTES --------------------------

data Route = PhotoboothsPage
           | EventsPage String String
           | StatisticsPage String String

derive instance genericRoute :: Generic Route

------ LENSES --------------------------

_collection :: forall a b o. Lens {collection :: a | o} {collection :: b | o} a b
_collection = lens _.collection (_ {collection = _})

_collectionEditing = lens (\{collection: a, editing: b} -> {collection: a, editing: b})
                           (\old {collection: a, editing: b} -> old {collection = a, editing = b})

_collectionEditingD = lens (\{collection: a, editing: b, deleting: c} -> {collection: a, editing: b, deleting: c})
                            (\old {collection: a, editing: b, deleting: c} -> old {collection = a, editing = b, deleting = c})

_new :: forall a b o. Lens {new :: a | o} {new :: b | o} a b
_new = lens _.new (_ {new = _})

_model :: forall a b o. Lens {model :: a | o} {model :: b | o} a b
_model = lens _.model (_ {model = _})

_state :: forall a b o. Lens {state :: a | o} {state :: b | o} a b
_state = lens _.state (_ {state = _})

_editing :: forall a b o. Lens {editing :: a | o} {editing :: b | o} a b
_editing = lens _.editing (_ {editing = _})

_deleting :: forall a b o. Lens {deleting :: a | o} {deleting :: b | o} a b
_deleting = lens _.deleting (_ {deleting = _})

_index :: forall a b o. Lens {index :: a | o} {index :: b | o} a b
_index = lens _.index (_ {index = _})

_previous :: forall a b o. Lens {previous :: a | o} {previous :: b | o} a b
_previous = lens _.previous (_ {previous = _})

_saving :: forall a b o. Lens {saving :: a | o} {saving :: b | o} a b
_saving = lens _.saving (_ {saving = _})

_route :: forall a b o. Lens {route :: a | o} {route :: b | o} a b
_route = lens _.route (_ {route = _})

_pbPage :: forall a b c d e o. LensP {photobooths :: a, profiles :: e, photoboothsPage :: {new :: b, editing :: c, deleting :: d} | o}
                                {collection :: a, profiles :: e, new :: b, editing :: c, deleting :: d}
_pbPage = lens (\obj -> {collection: obj.photobooths
                       , profiles: obj.profiles
                       , new: obj.photoboothsPage.new
                       , editing: obj.photoboothsPage.editing 
                       , deleting: obj.photoboothsPage.deleting
                       })
               (\old obj ->
                   old {photobooths = obj.collection
                       , profiles = obj.profiles
                       , photoboothsPage = {new: obj.new, editing: obj.editing, deleting: obj.deleting}
                       })

_eventsPage :: forall a b c d e o. LensP {events :: a, profiles :: e, eventsPage :: {new :: b, editing :: c, deleting :: d} | o}
                                {collection :: a, profiles :: e, new :: b, editing :: c, deleting :: d}
_eventsPage = lens (\obj -> {collection: obj.events
                            , profiles: obj.profiles
                            , new: obj.eventsPage.new
                            , editing: obj.eventsPage.editing
                            , deleting: obj.eventsPage.deleting
                            })
               (\old obj ->
                   old {events = obj.collection
                       , profiles = obj.profiles
                       , eventsPage = {new: obj.new, editing: obj.editing, deleting: obj.deleting}
                       })

_statisticsPage :: forall a b o. LensP {events :: a, statistics :: b | o}
                                       {events :: a, statistics :: b}
_statisticsPage = lens (\obj -> { events: obj.events
                                , statistics: obj.statistics })
                       (\old obj ->
                           old {events = obj.events
                               , statistics = obj.statistics })


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

_files :: forall a b o. Lens {files :: a | o} {files :: b | o} a b
_files = lens _.files (_ {files = _})

_eventId :: forall a b o. Lens {eventId :: a | o} {eventId :: b | o} a b
_eventId = lens _.eventId (_ {eventId = _})

_file :: forall a b o. Lens {file :: a | o} {file :: b | o} a b
_file = lens _.file (_ {file = _})

_profiles :: forall a b o. Lens {profiles :: a | o} {profiles :: b | o} a b
_profiles = lens _.profiles (_ {profiles = _})

_savingFile :: forall a b o. Lens {savingFile :: a | o} {savingFile :: b | o} a b
_savingFile = lens _.savingFile (_ {savingFile = _})

_statistics :: forall a b o. Lens {statistics :: a | o} {statistics :: b | o} a b
_statistics = lens _.statistics (_ {statistics = _})

_eventStatistics :: forall a b o. Lens {eventStatistics :: a | o} {eventStatistics :: b | o} a b
_eventStatistics = lens _.eventStatistics (_ {eventStatistics = _})

_monthlyStatistics :: forall a b o. Lens {monthlyStatistics :: a | o} {monthlyStatistics :: b| o} a b
_monthlyStatistics = lens _.monthlyStatistics (_ {monthlyStatistics = _})

_photoboothId :: forall a b o. Lens {photoboothId :: a | o} {photoboothId :: b | o} a b
_photoboothId = lens _.photoboothId (_ {photoboothId = _})

_month :: forall a b o. Lens {month :: a | o} {month :: b | o} a b
_month = lens _.month (_ {month = _})

_pictures :: forall a b o. Lens {pictures :: a | o} {pictures :: b | o} a b
_pictures = lens _.pictures (_ {pictures = _})

_prints :: forall a b o. Lens {prints :: a | o} {prints :: b | o} a b
_prints = lens _.prints (_ {prints = _})

_events :: forall a b o. Lens {events :: a | o} {events :: b | o} a b
_events = lens _.events (_ {events = _})
