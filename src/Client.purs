module Client where

import Prelude
import Klikhut.Model.Photobooth
import Klikhut.Async

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Components (textField)
import OpticUI (with, withView, traversal, ui, text, UI(), Markup(), animate, runHandler, inline, foreach)
import OpticUI.Components.Async (onResult, async)

import Network.HTTP.Affjax (AJAX())

import DOM (DOM())

import Data.Lens (lens, traversed, Lens(), set, over, view)
import Data.Lens.Common (_Just, _Nothing)
import Data.Foldable (Foldable, mconcat)
import Data.Array (snoc, (!!), updateAt)
import Data.Maybe (Maybe(..), maybe)

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (message, Error())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import Control.Apply ((*>))

import Klikhut.Endpoint

------ MAIN -----------

type State = forall eff. 
             { photobooths :: AsyncModel eff (Array Photobooth)
             , newPb :: {model :: {computername :: String, alias :: String, defaultprofile :: String}, 
                         state :: AsyncModel eff Unit}
                         , editing :: Maybe {index :: Int, previous :: Photobooth, saving :: AsyncModel eff Unit} }
type KlikUI eff a = UI eff Markup a a
type AjaxRef eff = (ajax :: AJAX, ref :: REF | eff)
type AjaxRefDom eff = (ajax :: AJAX, ref :: REF, dom :: DOM | eff)
type RefDom eff = (ref :: REF, dom :: DOM | eff)
 
initialState :: State
initialState = {photobooths: Initial, newPb: {model : {computername: "", alias: "", defaultprofile: ""}, state: Initial}, editing: Nothing}

main :: forall eff. Eff (ajax :: AJAX, ref :: REF, dom :: DOM | eff) Unit
main = animate initialState $ mconcat [
    photoboothsPage
  ]

------ UI's -----------

data PhotoboothsCommand = LoadAll
                        | SaveModel
                        | Loaded (Array Photobooth)
                        | LoadingFailed Error
                        | NewSaved 
                        | NewSaveFailed Error
                        | StartEdit Int
                        | CancelEdit
                        | SaveEdit
                        | EditSaved
                        | EditSaveFailed Error

photoboothsPage :: forall eff. KlikUI (AjaxRefDom eff) { photobooths :: AsyncModel (AjaxRefDom eff) (Array Photobooth)
                                                       , newPb :: {model :: {computername :: String, alias :: String, defaultprofile :: String}, 
                                                                   state :: AsyncModel (AjaxRefDom eff) Unit}
                                                       , editing :: Maybe {index :: Int, previous :: Photobooth, saving :: AsyncModel (AjaxRefDom eff) Unit}}
photoboothsPage = with $ \s h ->
                    let handle LoadAll = async loadPbs >>= \a -> runHandler h (set _photobooths (Busy a) s)
                        handle SaveModel = async (saveNewPb (Photobooth (view (_newPb <<< _model) s))) >>= \a -> runHandler h (set (_newPb <<< _state) (Busy a) s)
                        handle (Loaded pbs) = runHandler h (set _photobooths (Done pbs) s)
                        handle (LoadingFailed err) = runHandler h (set _photobooths (Errored err) s)
                        handle (NewSaveFailed err) = runHandler h (set (_newPb <<< _state) (Errored err) s)
                        handle NewSaved = runHandler h $ updates s
                          where
                            new = Photobooth $ view (_newPb <<< _model) s
                            updates = over (_photobooths <<< _Done) (\arr -> snoc arr new) <<< 
                                           set (_newPb <<< _model) {computername: "", alias: "", defaultprofile: ""} <<< 
                                           set (_newPb <<< _state) (Done unit) 
                        handle (StartEdit i) = case (view (_photobooths <<< _Done) s) !! i of
                                                    Nothing -> return unit
                                                    Just a -> runHandler h (set _editing (Just {index: i, previous: a, saving: Initial}) s)
                        handle CancelEdit = case view _editing s of
                                                 Nothing -> return unit
                                                 Just {index: ind, previous: old} -> runHandler h $ (set _editing Nothing <<<
                                                                                                     over (_photobooths <<< _Done) (\pbs -> maybe pbs id (updateAt ind old pbs))) s
                        handle SaveEdit = case view _editing s of
                                               Nothing -> return unit
                                               Just {index: ind} -> case view (_photobooths) s of
                                                                         Done pbs -> case pbs !! ind of 
                                                                                          Just pb -> async (updatePB pb) >>= \a -> runHandler h (set (_editing <<< _Just <<< _saving) (Busy a) s)
                                                                                          _ -> return unit
                                                                         _ -> return unit
                        handle EditSaved = runHandler h $ set (_editing) Nothing s
                        handle (EditSaveFailed err) = runHandler h $ set (_editing <<< _Just <<< _saving) (Errored err) s
                    in mconcat [
                      withView (H.table [H.classA "table"] <<< H.tbody []) $ mconcat [
                          ui $ H.tr [] $ mconcat [H.th [] $ H.text "Name", H.th [] $ H.text "Alias", H.th [] $ H.text "Default Profile"],
                          _photoboothsEditing (listPhotobooths handle (maybe Nothing (\ed -> Just ed.index) (view _editing s))),
                          _newPb $ _model $ makeNewPbInputs
                        ],
                      _newPb $ _state $ (makeNewPbButton handle)
                    ]

makeNewPbInputs :: forall eff. KlikUI (AjaxRefDom eff) {computername :: String, alias :: String, defaultprofile :: String}
makeNewPbInputs = with \model h -> withView (H.tr []) $ mconcat [
  withView (H.td []) $ _computername $ textField [],
  withView (H.td []) $ _alias $ textField [],
  withView (H.td []) $ _defaultprofile $ textField []
]

makeNewPbButton :: forall eff. (PhotoboothsCommand -> Eff (ref :: REF | eff) Unit) -> KlikUI (ref :: REF | eff) (AsyncModel (ref :: REF | eff) Unit) 
makeNewPbButton handle = 
  with \newPb h -> mconcat [
       _Initial $ ui $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle SaveModel] $ text "Save shit",
       _Busy $ mconcat [
           ui $ H.button [H.classA "btn btn-warning"] $ text "Saving PB",
           onResult (\_ -> handle NewSaved) (handle <<< NewSaveFailed)
         ],
       _Done $ ui $ H.button [H.classA "btn btn-success", H.onClick \_ -> handle SaveModel] $ text "Saved!",
       _Errored $ with \err _ -> mconcat [
           ui $ H.button [H.classA "btn btn-danger"] $ text "Failed!",
           ui $ H.div [H.classA "alert alert-danger"] $ text ("Photobooths failed to get saved: " <> message err)
         ]
      ]

listPhotobooths :: forall eff obj. (PhotoboothsCommand -> Eff (RefDom eff) Unit) -> Maybe Int
                   -> KlikUI (RefDom eff) {photobooths :: (AsyncModel (RefDom eff) (Array Photobooth)), editing :: Maybe {saving :: AsyncModel (RefDom eff) Unit | obj}} 
listPhotobooths handle selInd = mconcat [
  _photobooths $ with \pbs h -> mconcat [
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
    withView (H.td []) $ (_Photobooth <<< _alias) $ textField [],
    withView (H.td []) $ (_Photobooth <<< _defaultprofile) $ textField [],
    withView (H.td []) $ mconcat [
      ui $ H.button [H.classA "btn btn-warning", H.onClick \_ -> handle CancelEdit] $ text "Cancel",
      ui $ H.button [H.classA "btn btn-success", H.onClick \_ -> handle SaveEdit] $ text "Save"
      ]
  ]
showPB handle (Just selInd) i = with \(Photobooth pb) h -> 
  withView (H.tr []) $ mconcat [
    ui $ H.td [] $ text pb.computername,
    ui $ H.td [] $ text pb.alias,
    ui $ H.td [] $ text pb.defaultprofile
  ]
showPB handle _ i = with \(Photobooth pb) h ->
  withView (H.tr []) $ mconcat [
    ui $ H.td [] $ text pb.computername,
    ui $ H.td [] $ text pb.alias,
    ui $ H.td [] $ text pb.defaultprofile,
    ui $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle (StartEdit i)] $ text "Edit"
  ]

------ AFF -------------

loadPbs :: forall eff. Aff (ajax :: AJAX | eff) (Array Photobooth)
loadPbs = execEndpoint getPhotobooths unit unit

saveNewPb :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Unit
saveNewPb pb = execEndpoint postPhotobooths unit pb

updatePB :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Unit
updatePB pb = execEndpoint putPhotobooths unit pb

------ LENSES ----------

_photobooths :: forall a b o. Lens {photobooths :: a | o} {photobooths :: b | o} a b
_photobooths = lens _.photobooths (_ {photobooths = _})

_photoboothsEditing = lens (\{photobooths: a, editing: b} -> {photobooths: a, editing: b})
                           (\old {photobooths: a, editing: b} -> old {photobooths = a, editing = b})

_newPb :: forall a b o. Lens {newPb :: a | o} {newPb :: b | o} a b
_newPb = lens _.newPb (_ {newPb = _})

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
