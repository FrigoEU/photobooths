module Klikhut.Client.PhotoboothsPage (photoboothsPage) where

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Components (textField)
import OpticUI (with, withView, traversal, ui, text, UI(), Markup(), runHandler, inline, foreach)
import OpticUI.Components.Async (onResult, async)

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (message, Error())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import Control.Apply ((*>))

import Data.Lens (lens, traversed, Lens(), set, over, view)
import Data.Lens.Common (_Just, _Nothing)
import Data.Foldable (Foldable, mconcat)
import Data.Array (snoc, (!!), updateAt)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)

import Prelude
import DOM (DOM())
import Network.HTTP.Affjax (AJAX())

import Klikhut.Model.Photobooth
import Klikhut.Endpoint
import Klikhut.Types
import Klikhut.Async
import ClientState

------ PHOTOBOOTH UI's -----------

data PhotoboothsCommand = LoadAll
                        | Loaded (Array Photobooth)
                        | LoadingFailed Error
                        | SaveNew
                        | NewSaved 
                        | NewSaveFailed Error
                        | StartEdit Int
                        | CancelEdit
                        | SaveEdit
                        | EditSaved
                        | EditSaveFailed Error
                        | ToEvents String

photoboothsPage :: forall eff. KlikUI (AjaxRefDom eff) { collection :: AsyncModel (AjaxRefDom eff) (Array Photobooth)
                                                       , route :: Route
                                                       , new :: {model :: {computername :: String, alias :: String, defaultprofile :: String}, 
                                                                   state :: AsyncModel (AjaxRefDom eff) Unit}
                                                       , editing :: Maybe {index :: Int, previous :: Photobooth, saving :: AsyncModel (AjaxRefDom eff) Unit}}
photoboothsPage = with $ \s h ->
  let handle LoadAll = async loadPbs >>= \a -> runHandler h (set _collection (Busy a) s)
      handle SaveNew = async (saveNewPb (Photobooth (view (_new <<< _model) s))) >>= \a -> runHandler h (set (_new <<< _state) (Busy a) s)
      handle (Loaded pbs) = runHandler h (set _collection (Done pbs) s)
      handle (LoadingFailed err) = runHandler h (set _collection (Errored err) s)
      handle (NewSaveFailed err) = runHandler h (set (_new <<< _state) (Errored err) s)
      handle NewSaved = runHandler h $ updates s
        where
          new = Photobooth $ view (_new <<< _model) s
          updates = over (_collection <<< _Done) (\arr -> snoc arr new) <<< 
                         set (_new <<< _model) {computername: "", alias: "", defaultprofile: ""} <<< 
                         set (_new <<< _state) (Initial) 
      handle (StartEdit i) = maybe (return unit) 
                                   (\pb -> runHandler h (set _editing (Just {index: i, previous: pb, saving: Initial}) s)) 
                                   (view (_collection <<< _Done) s !! i) 
      handle CancelEdit = maybe (return unit) 
                                (\{index: ind, previous: old} -> let updates = set _editing Nothing <<<
                                                                               over (_collection <<< _Done) (\pbs -> maybe pbs id (updateAt ind old pbs))
                                                                  in runHandler h $ updates s)
                                (view _editing s)
      handle SaveEdit = maybe (return unit) 
                              (\pb -> async (updatePB pb) >>= \a -> runHandler h (set (_editing <<< _Just <<< _saving) (Busy a) s))
                              (view _editing s >>= \{index: ind} -> view (_collection <<< _Done) s !! ind)
      handle EditSaved = runHandler h $ set (_editing) Nothing s
      handle (EditSaveFailed err) = runHandler h $ set (_editing <<< _Just <<< _saving) (Errored err) s
      handle (ToEvents name) = runHandler h $ set _route (EventsPage name) s
  in withView (H.div [H.classA "container"] <<< H.table [H.classA "table crud-table"] <<< H.tbody []) $ mconcat [
        ui $ H.tr [] $ mconcat [H.th [] $ H.text "Name", H.th [] $ H.text "Alias", H.th [] $ H.text "Default Profile", H.th [] $ H.text "Actions", H.th [] $ H.text "Link"],
        _collectionEditing (listPhotobooths handle (view _editing s >>= \ed -> return ed.index)),
        _new $ (makeNewPb handle)
      ]

makeNewPb :: forall eff. (PhotoboothsCommand -> Eff (RefDom eff) Unit) 
                               -> KlikUI (RefDom eff) { model :: {computername :: String, alias :: String, defaultprofile :: String}
                                                          , state :: AsyncModel (RefDom eff) Unit}
makeNewPb handle = with \model h -> withView (H.tr []) $ mconcat [
  withView (H.td []) $ (_model <<< _computername) $ textField [H.classA "form-control"],
  withView (H.td []) $ (_model <<< _alias) $ textField [H.classA "form-control"],
  withView (H.td []) $ (_model <<< _defaultprofile) $ textField [H.classA "form-control"],
  withView (H.td []) $ _state $ (makeNewPbButton handle)
]

makeNewPbButton :: forall eff. (PhotoboothsCommand -> Eff (ref :: REF | eff) Unit) -> KlikUI (ref :: REF | eff) (AsyncModel (ref :: REF | eff) Unit) 
makeNewPbButton handle = 
  with \new h -> mconcat [
       _Initial $ ui $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle SaveNew] $ text "Save shit",
       _Busy $ mconcat [
           ui $ H.button [H.classA "btn btn-warning"] $ text "Saving PB",
           onResult (\_ -> handle NewSaved) (handle <<< NewSaveFailed)
         ],
       _Errored $ with \err _ -> mconcat [
           ui $ H.button [H.classA "btn btn-danger"] $ text "Failed!",
           ui $ H.div [H.classA "alert alert-danger"] $ text ("Photobooths failed to get saved: " <> message err)
         ]
      ]

listPhotobooths :: forall eff obj. (PhotoboothsCommand -> Eff (RefDom eff) Unit) -> Maybe Int
                   -> KlikUI (RefDom eff) {collection :: (AsyncModel (RefDom eff) (Array Photobooth)), editing :: Maybe {saving :: AsyncModel (RefDom eff) Unit | obj}} 
listPhotobooths handle selInd = mconcat [
  _collection $ with \pbs h -> mconcat [
       _Initial $ inline (handle LoadAll *> (return $ H.tr [] $ H.td [] $ text "Nothing loaded yet")),
       _Busy $ mconcat [
         ui $ H.tr [] $ H.td [] $ text "Loading photobooths",
         onResult (handle <<< Loaded) (handle <<< LoadingFailed)
       ],
       _Done $ foreach (showPB handle selInd),
       _Errored $ with \err _ -> ui $ H.tr [] $ H.td [] $ text ("Photobooths failed to load: " <> message err)],
  (_editing <<< _Just <<< _saving <<< _Busy) $ onResult (\_ -> handle EditSaved) (handle <<< EditSaveFailed)
  ]

showPB :: forall eff. (PhotoboothsCommand -> Eff (dom :: DOM | eff) Unit) -> Maybe Int -> Int -> KlikUI (dom :: DOM | eff) Photobooth
showPB handle (Just selInd) i | i == selInd = with \(Photobooth pb) h -> 
  withView (H.tr []) $ mconcat [
    ui $ H.td [] $ text pb.computername,
    withView (H.td []) $ (_Photobooth <<< _alias) $ textField [H.classA "form-control"],
    withView (H.td []) $ (_Photobooth <<< _defaultprofile) $ textField [H.classA "form-control"],
    ui $ H.td [] $ mconcat [
      H.button [H.classA "btn btn-warning", H.onClick \_ -> handle CancelEdit] $ text "Cancel",
      H.button [H.classA "btn btn-success", H.onClick \_ -> handle SaveEdit] $ text "Save"
      ]
  ]
showPB handle a i = with \(Photobooth pb) h -> 
  ui $ H.tr [] $ mconcat [
    H.td [] $ text pb.computername,
    H.td [] $ text pb.alias,
    H.td [] $ text pb.defaultprofile,
    maybe (H.td [] $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle (StartEdit i)] $ text "Edit") (const mempty) a,
    H.td [] $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle (ToEvents pb.computername)] $ text "Zie events"
  ]

------ AFF -------------

loadPbs :: forall eff. Aff (ajax :: AJAX | eff) (Array Photobooth)
loadPbs = execEndpoint getPhotobooths unit unit

saveNewPb :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Unit
saveNewPb pb = execEndpoint postPhotobooths unit pb

updatePB :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Unit
updatePB pb = execEndpoint putPhotobooths unit pb
