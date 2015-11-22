module App.GUI.Views.EventsPage (eventsPage) where

import Prelude

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Components (textField)
import OpticUI (with, withView, traversal, ui, text, UI(), Markup(), runHandler, foreach)
import OpticUI.Components.Async (onResult, async)

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (message, Error())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import Control.Apply ((*>))

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Date (Date(), Now(), now)
import Data.Foldable (mconcat)
import Data.Lens.Common
import Data.Lens (view, over)
import Data.Lens.Traversal (traversed)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Data.Array (cons)
import Data.StrMap (StrMap(), lookup)

import Prelude
import DOM (DOM())
import DOM.Timer(Timer(), timeout)
import DOM.File.Types (File())
import Network.HTTP.Affjax (AJAX())

import App.Model.Event
import App.Model.Date
import App.Model.SavedImage
import App.Model.Async
import App.Model.Profile
import App.GUI.Router
import App.GUI.Types
import App.GUI.State
import App.GUI.Components.Exec
import App.GUI.Components.FileInput
import App.GUI.Components.Select
import App.GUI.Components.DateTimeField
import App.GUI.Views.Crud
import App.GUI.Views.Profiles
import App.Endpoint
import App.GUI.Load

data EventsCommand eff = Crud (CrudCommand (EventWithState eff))

eventsPage :: forall eff. String ->
                          AppUI (ANDRT eff) 
                          { collection :: AsyncModel (ANDRT eff) (Array (EventWithState (ANDRT eff)))
                          , profiles :: AsyncModel (ANDRT eff) Profiles
                          , route :: Route
                          , new :: { model :: {id :: Maybe Int, computername :: String, name :: String, datefrom :: Date, dateuntil :: Date, profile :: String, images :: Array SavedImage}
                                   , state :: AsyncModel (ANDRT eff) (EventWithState (ANDRT eff))}
                          , editing :: Maybe {index :: Int, previous :: (EventWithState (ANDRT eff)), saving :: AsyncModel (ANDRT eff) (EventWithState (ANDRT eff))}}
eventsPage cn = with c
  where 
    c s h =
      let impls = { loadAll: loadEvents cn, saveNew: saveNewEvent, saveEdit: saveUpdatedEvent 
                  , initial: (now >>= \d -> return {id: Nothing, computername: cn, name: "", datefrom: d, dateuntil: d, profile: "", images: []})
                  , constr: (\a ->{model: Event a, state: {savingImage: Initial, image: Nothing}})}
          handle (Crud command) = crudHandler s h impls command
       in withView (H.div [H.classA ""]) $ mconcat [
           ui $ H.h1 [] $ H.text ("Events for: " <> cn),
           withView (H.table [H.classA "table crud-table"] <<< H.tbody []) ((_collectionEditing $ showEvents handle (view (_profiles <<< _Done) s)) <> (_new $ makeNewEvent handle)),
           _profiles loadProfiles
         ]

-------- Existing + Edit -------------------

showEvents :: forall eff. (EventsCommand (ANDRT eff) -> Eff (ANDRT eff) Unit)
                          -> StrMap (Array String)
                          -> AppUI (ANDRT eff) { collection :: AsyncModel (ANDRT eff) (Array (EventWithState (ANDRT eff)))
                                                , editing :: Maybe { index :: Int, previous :: (EventWithState (ANDRT eff))
                                                                   , saving :: AsyncModel (ANDRT eff) (EventWithState (ANDRT eff))}}
showEvents handle profiles = with c 
                 <> (_collection <<< _Initial                 $ exec $ handle (Crud LoadAll))
                 <> (_collection <<< _Busy                    $ onResult (handle <<< Crud <<< Loaded) (handle <<< Crud <<< LoadingFailed)) 
                 <> (_editing <<< _Just <<< _saving <<< _Busy $ onResult (handle <<< Crud <<< EditSaved) (handle <<< Crud <<< EditSaveFailed))
  where 
    c {collection: Initial}   h = ui $ H.tr [] $ H.td [] $ H.text "No events loaded yet, loading..."
    c {collection: Busy _}    h = ui $ H.tr [] $ H.td [] $ H.text "Loading events" 
    c {collection: Errored e} h = ui $ H.tr [] $ H.td [] $ H.div [H.classA "alert alert-danger"] $ H.text ("Events failed to load: " <> message e)
    c s@{collection: Done _}  h = let selI = maybe Nothing (\ed -> Just ed.index) s.editing
                                      editing = view _editing s
                                   in (_collection <<< _Done) $ foreach (\i -> withView (H.tr []) $ with (line selI editing i))
    line (Just selI) editing i ({model: Event ev}) h | selI == i = mconcat [ ui $ H.td [] $ H.text $ maybe "" show ev.id
                                                                           , ui $ H.td [] $ H.text $ ev.computername
                                                                           , withView (H.td []) $ (_model <<< _Event <<< _name) $ textField [H.classA "form-control"]
                                                                           , withView (H.td []) $ (_model <<< _Event <<< _datefrom) $ dateTimeField [H.classA "form-control"]
                                                                           , withView (H.td []) $ (_model <<< _Event <<< _dateuntil) $ dateTimeField [H.classA "form-control"]
                                                                           , withView (H.td []) $ (_model <<< _Event <<< _profile) $ select (fromMaybe [] $ lookup ev.computername profiles) 
                                                                                                                                            id [H.classA "form-control"]
                                                                           , editEventButton handle i editing
                                                                           , ui $ H.td [] $ H.text ""
                                                                           ] 
    line _ editing i s@({model: Event ev, state: st}) h = 
      let fileSelected _ Nothing = return unit
          fileSelected _ (Just f) = async (saveFile f (maybe (-1) id ev.id)) >>= (\a -> runHandler h (s {state = (st {savingImage = Busy a})}))
          fileSaved si = runHandler h { model: over (_Event <<< _images) (cons si) s.model
                                      , state: st {savingImage = Initial, image = Nothing}}
          fileSaveErrored err = runHandler h (s {state = (st {savingImage = Errored err})})
       in mconcat [ ui $ H.td [] $ H.text $ maybe "" show ev.id
                  , ui $ H.td [] $ H.text $ ev.computername
                  , ui $ H.td [] $ H.text $ ev.name
                  , ui $ H.td [] $ H.text $ iso8601 ev.datefrom
                  , ui $ H.td [] $ H.text $ iso8601 ev.dateuntil
                  , ui $ H.td [] $ H.text $ ev.profile
                  , withView (H.td []) $ editEventButton handle i editing
                  , withView (H.td []) $ mconcat [ (_state <<< _image) $ fileInput [onFileInput fileSelected, accept ".jpg,.jpeg"]
                                                 , (_model <<< _Event <<< _images) $ traversal traversed (with \(SavedImage si) _ -> ui $ H.div [] $ H.text si.name)
                                                 , (_state <<< _savingImage <<< _Errored) $ with (\err _ -> ui $ H.div [H.classA "alert alert-danger"] $ H.text $ message err)
                                                 ] 
                  , (_state <<< _savingImage <<< _Busy) $ onResult fileSaved fileSaveErrored
                  ] 


editEventButton :: forall eff a b c. (EventsCommand eff -> Eff eff Unit) -> Int -> Maybe {index :: Int, saving :: AsyncModel eff a | b} -> AppUI eff c
editEventButton handle i editing = c editing
  where 
    c Nothing                          = ui $ H.button [H.classA "btn btn-primary", H.onClick (const $ handle $ Crud $ StartEdit i)] $ H.text "Edit!"
    c (Just {index: selI}) | selI /= i = ui $ H.div [] $ H.text ""
    c (Just {saving: Busy _})          = (ui $ H.button [H.classA "btn btn-warning"] $ H.text "Saving")
    c (Just {saving: Errored err})     = (ui $ H.button [H.classA "btn btn-danger", H.onClick \_ -> handle (Crud SaveEdit)] $ H.text "Save failed, try again")
                                      <> (ui $ H.div [H.classA "alert alert-danger"] $ H.text $ message err)
    c (Just {saving: _})               = (ui $ H.button [H.classA "btn btn-success", H.onClick \_ -> handle (Crud SaveEdit)] $ H.text "Save edit")
                                      <> (ui $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle (Crud CancelEdit)] $ H.text "Cancel edit")

-------- New -------------------------------

makeNewEvent :: forall eff. (EventsCommand (ANDR eff) -> Eff (ANDR eff) Unit)
                            -> AppUI (ANDR eff) 
                            { model :: {id :: Maybe Int, computername :: String, name :: String, datefrom :: Date, dateuntil :: Date, profile :: String, images :: Array SavedImage}
                            , state :: AsyncModel (ANDR eff) (EventWithState (ANDR eff))}
makeNewEvent handle = 
  with \s h -> withView (H.tr []) $ mconcat [ 
                            ui $ H.td [] $ H.text "",
                            withView (H.td []) $ (_model <<< _computername) $ textField [H.classA "form-control"],
                            withView (H.td []) $ (_model <<< _name) $ textField [H.classA "form-control"],
                            withView (H.td []) $ (_model <<< _datefrom) $ dateTimeField [H.classA "form-control"],
                            withView (H.td []) $ (_model <<< _dateuntil) $ dateTimeField [H.classA "form-control"],
                            ui $ H.td [] $ H.text "",
                            {-- withView (H.td []) $ (_model <<< _profile) $ textField [H.classA "form-control"], --}
                            withView (H.td []) $ _state $ (newEventButton handle),
                            ui $ H.td [] $ H.text ""
                          ]

newEventButton :: forall eff. (EventsCommand  (ref :: REF | eff)-> Eff (ref :: REF | eff) Unit)
                              -> AppUI (ref :: REF | eff) (AsyncModel (ref :: REF | eff) (EventWithState (ref :: REF | eff)))
newEventButton handle = with c
  where 
    c (Busy _) h = (ui $ H.button [H.classA "btn btn-warning"] $ H.text "Saving Event")
                <> (_Busy $ onResult (handle <<< Crud <<< NewSaved) (handle <<< Crud <<< NewSaveFailed))
    c (Errored err) h = (ui $ H.button [H.classA "btn btn-danger", H.onClick \_ -> handle $ Crud SaveNew] 
                              $ H.text "Saving Failed, Try Again?")
                     <> (ui $ H.div [H.classA "alert alert-danger"] $ text ("Event not saved: " <> message err))
    c _ h = ui $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle $ Crud SaveNew] $ H.text "Save!"

--------- Aff ----------------------------

saveNewEvent :: forall eff a. {model :: Event | a} -> Aff (ajax :: AJAX | eff) {model :: Event | a}
saveNewEvent i = execEndpoint postEvents unit (view _model i) >>= \n -> return $ i {model = n}

saveUpdatedEvent :: forall eff a. {model :: Event | a} -> Aff (ajax :: AJAX | eff) {model :: Event | a}
saveUpdatedEvent i = execEndpoint putEvents unit (view _model i) >>= \n -> return $ i {model = n}

saveFile :: forall eff. File -> Int -> Aff (ajax :: AJAX | eff) SavedImage
saveFile file i = sendJpeg attachImage file (Tuple i (name file))
