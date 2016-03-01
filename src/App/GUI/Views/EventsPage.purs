module App.GUI.Views.EventsPage (eventsPage) where

import Prelude

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Components (textField)
import OpticUI (with, withView, traversal, ui, runHandler, foreach)
import OpticUI.Components.Async (onResult, async)

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff (Eff())

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Date (Date(), now)
import Data.Foldable (mconcat)
import Data.Lens.Common
import Data.Lens (view, over)
import Data.Lens.Traversal (traversed)
import Data.Tuple (Tuple(..))
import Data.Array (cons, length)
import Data.StrMap (StrMap(), lookup)

import DOM.File.Types (File())
import Network.HTTP.Affjax (AJAX())

import App.Model.Event
import App.Model.Date
import App.Model.SavedFile
import App.Model.Async
import App.Model.Profile
import App.GUI.Types
import App.GUI.State
import App.GUI.Components.Exec
import App.GUI.Components.FileInput
import App.GUI.Components.Select
import App.GUI.Components.DateTimeField
import App.GUI.Components.Markup
import App.GUI.Views.Crud
import App.GUI.Views.Profiles
import App.Endpoint
import App.GUI.Load
import App.GUI.Router
import App.GUI.Components.CrudButtons

data EventsCommand eff = Crud (CrudCommand (EventWithState eff))

eventsPage :: forall eff. String ->
                          String ->
                          Int ->
                          Nav (ANDRT eff) ->
                          AppUI (ANDRT eff) 
                          { collection :: AsyncModel (ANDRT eff) (Array (EventWithState (ANDRT eff)))
                          , profiles :: AsyncModel (ANDRT eff) Profiles
                          , new :: { model :: {id :: Maybe Int, computername :: String, name :: String, datefrom :: Date, dateuntil :: Date, profile :: String, files :: Array SavedFile}
                                   , state :: AsyncModel (ANDRT eff) (EventWithState (ANDRT eff))}
                          , editing :: Maybe {index :: Int, previous :: (EventWithState (ANDRT eff)), saving :: AsyncModel (ANDRT eff) (EventWithState (ANDRT eff))}
                          , deleting :: Maybe {index :: Int, saving :: AsyncModel (ANDRT eff) Unit}}
eventsPage cn alias page nav = with c
  where c s h =
          let impls = { loadAll: loadEventsWithState cn page, saveNew: saveNewEvent, saveEdit: saveUpdatedEvent, delete: const (return unit)
                      , initial: (now >>= \d -> return {id: Nothing, computername: cn, name: "", datefrom: d, dateuntil: d, profile: "", files: []})
                      , constr: (\a ->{model: Event a, state: {savingFile: Initial, file: Nothing}})}
              handle (Crud command) = crudHandler s h impls command
              pageSize = 20
           in mconcat [ ui $ pageTitle (mconcat [ H.button [ H.classA "btn-nav" , H.onClick \_ -> nav  PhotoboothsPage] (H.text "^")
                                                , H.text " Events for: " , H.em [] (H.text alias) 
                                                , H.text ", page " , H.em [] (H.text $ show $ page + 1)
                                                , H.button [ H.classA ("btn-nav" <> if (page > 0) then "" else " hide")
                                                           , H.onClick \_ -> nav $ EventsPage cn alias (page - 1)] (H.text "<")
                                                , H.button [ H.classA ("btn-nav" <> if (length (view (_collection <<< _Done) s) == pageSize) then "" else " hide")
                                                           , H.onClick \_ -> nav $ EventsPage cn alias (page + 1)] (H.text ">") 
                                                ])
                      , withView crudTable $ mconcat [ ui $ tableHeader [H.classA "indexed-tr"] ["" , "Computer" , "Name" , "Start" , "End" , "Profile" , "Actions" , "Files"]
                                                     , _new $ makeNewEvent handle
                                                     , _collectionEditing $ showEvents handle (view (_profiles <<< _Done) s) 
                                                     ]
                      , _profiles loadProfiles
                      ]

-------- List -------------------
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
    line (Just selI) editing i ({model: Event ev}) h | selI == i = mconcat $ withView (H.td []) <$> [ ui $ H.text $ maybe "" show ev.id
                                                                                                    , ui $ H.text $ ev.computername
                                                                                                    , (_model <<< _Event <<< _name) $ textField [H.classA "form-control"]
                                                                                                    , (_model <<< _Event <<< _datefrom) $ dateTimeField [H.classA "form-control"]
                                                                                                    , (_model <<< _Event <<< _dateuntil) $ dateTimeField [H.classA "form-control"]
                                                                                                    , (_model <<< _Event <<< _profile) $ select (fromMaybe [] $ lookup ev.computername profiles) id 
                                                                                                                                                [H.classA "form-control"]
                                                                                                    , editButton (handle <<< Crud) i editing
                                                                                                    , (ui $ H.br [] $ H.text "") <> listFiles
                                                                                                    ] 
    line _ editing i s@({model: Event ev, state: st}) h = 
      let fileSelected _ Nothing = return unit
          fileSelected _ (Just f) = async (saveFile f (maybe (-1) id ev.id)) >>= (\a -> runHandler h (s {state = (st {savingFile = Busy a})}))
          fileSaved si = runHandler h { model: over (_Event <<< _files) (cons si) s.model
                                      , state: st {savingFile = Initial, file = Nothing}}
          fileSaveErrored err = runHandler h (s {state = (st {savingFile = Errored err})})
          fileIdStr = maybe "" show ev.id
          fileInputId = "event-fileinput-" <> fileIdStr
       in mconcat [ ui $ H.td [] $ H.text fileIdStr
                  , ui $ H.td [] $ H.text ev.computername
                  , ui $ H.td [] $ H.text ev.name
                  , ui $ H.td [] $ H.text $ toLocalDatetime ev.datefrom
                  , ui $ H.td [] $ H.text $ toLocalDatetime ev.dateuntil
                  , ui $ H.td [] $ H.text ev.profile
                  , withView (H.td []) $ editButton (handle <<< Crud) i editing
                  , withView (H.td []) $ mconcat [ (_state <<< _file) (mconcat [ ui $ H.label [H.attr "for" fileInputId, H.classA "btn-action btn"] $ H.text "Upload"
                                                                               , fileInput [onFileInput fileSelected, H.attr "id" fileInputId]])
                                                 , listFiles
                                                 , (_state <<< _savingFile <<< _Errored) $ with (\err _ -> ui $ H.div [H.classA "alert alert-danger"] $ H.text $ message err)
                                                 ] 
                  , (_state <<< _savingFile <<< _Busy) $ onResult fileSaved fileSaveErrored
                  ] 
    listFiles = (_model <<< _Event <<< _files) $ traversal traversed (with \(SavedFile si) _ -> ui $ H.div [] $ H.text si.name)

-------- New -------------------------------

makeNewEvent :: forall eff. (EventsCommand (ANDR eff) -> Eff (ANDR eff) Unit)
                            -> AppUI (ANDR eff) 
                                     { model :: {id :: Maybe Int, computername :: String, name :: String, datefrom :: Date, dateuntil :: Date, profile :: String, files :: Array SavedFile}
                                     , state :: AsyncModel (ANDR eff) (EventWithState (ANDR eff))}
makeNewEvent handle = 
  with \s h -> rowUI [ ui $ H.text ""
                     , ui $ H.text s.model.computername
                     , (_model <<< _name) $ textField [H.classA "form-control"]
                     , (_model <<< _datefrom) $ dateTimeField [H.classA "form-control"]
                     , (_model <<< _dateuntil) $ dateTimeField [H.classA "form-control"]
                     , ui $ H.text ""
                     , _state $ (newButton (handle <<< Crud))
                     , ui $ H.text ""
                     ]

--------- Aff ----------------------------

saveNewEvent :: forall eff a. {model :: Event | a} -> Aff (ajax :: AJAX | eff) {model :: Event | a}
saveNewEvent i = execEndpoint postEvents unit (view _model i) >>= \n -> return $ i {model = n}

saveUpdatedEvent :: forall eff a. {model :: Event | a} -> Aff (ajax :: AJAX | eff) {model :: Event | a}
saveUpdatedEvent i = execEndpoint putEvents unit (view _model i) >>= \n -> return $ i {model = n}

saveFile :: forall eff. File -> Int -> Aff (ajax :: AJAX | eff) SavedFile
saveFile file i = execFileUploadEndpoint attachFile file (Tuple i (name file))
