module App.Client.Views.PhotoboothsPage (photoboothsPage) where

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Components (textField)
import OpticUI (with, withView, traversal, ui, text, UI(), Markup(), runHandler, foreach, Handler())
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
import DOM.Timer(Timer(), timeout)
import Network.HTTP.Affjax (AJAX())

import App.Model.Photobooth
import App.Model.Async
import App.Client.Router
import App.Client.Types
import App.Client.State
import App.Client.Components.Exec
import App.Client.Views.Crud
import App.Endpoint


------ PHOTOBOOTH UI's -----------

data PhotoboothsCommand = Crud (CrudCommand Photobooth)
                        | ToEvents String

photoboothsPage :: forall eff. Nav (AjaxRefDomTimer eff) ->
                               AppUI (AjaxRefDomTimer eff) { collection :: AsyncModel (AjaxRefDomTimer eff) (Array Photobooth)
                                                           , route :: Route
                                                           , new :: { model :: {id :: Maybe Int, computername :: String, alias :: String, defaultprofile :: String}
                                                                    , state :: AsyncModel (AjaxRefDomTimer eff) Photobooth}
                                                           , editing :: Maybe {index :: Int, previous :: Photobooth, saving :: AsyncModel (AjaxRefDomTimer eff) Photobooth}}
photoboothsPage goto = with $ \s h ->
  let impls = {loadAll: loadPbs, saveNew: saveNewPb, saveEdit: updatePB, initial: return {id: Nothing, computername: "", alias: "", defaultprofile: ""}, constr: Photobooth } 
      handle (Crud a) = crudHandler s h impls a
      handle (ToEvents name) = goto (EventsPage name)
  in withView (H.div [H.classA ""] <<< H.table [H.classA "table crud-table"] <<< H.tbody []) $ mconcat [
        ui $ H.tr [] $ mconcat [H.th [] $ H.text "Name", H.th [] $ H.text "Alias", H.th [] $ H.text "Default Profile", H.th [] $ H.text "Actions", H.th [] $ H.text "Link"],
        _collectionEditing (listPhotobooths handle (view _editing s >>= \ed -> return ed.index)),
        _new $ (makeNewPb handle)
      ]

makeNewPb :: forall eff. (PhotoboothsCommand -> Eff (RefDom eff) Unit) 
                         -> AppUI (RefDom eff) { model :: {id :: Maybe Int, computername :: String, alias :: String, defaultprofile :: String}
                                                , state :: AsyncModel (RefDom eff) Photobooth}
makeNewPb handle = with \model h -> withView (H.tr []) $ mconcat [
  withView (H.td []) $ (_model <<< _computername) $ textField [H.classA "form-control"],
  withView (H.td []) $ (_model <<< _alias) $ textField [H.classA "form-control"],
  withView (H.td []) $ (_model <<< _defaultprofile) $ textField [H.classA "form-control"],
  withView (H.td []) $ _state $ (makeNewPbButton handle)
]

makeNewPbButton :: forall eff. (PhotoboothsCommand -> Eff (ref :: REF | eff) Unit) -> AppUI (ref :: REF | eff) (AsyncModel (ref :: REF | eff) Photobooth) 
makeNewPbButton handle = 
  with \new h -> mconcat [
       _Initial $ ui $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle $ Crud SaveNew] $ text "Save shit",
       _Busy $ mconcat [
           ui $ H.button [H.classA "btn btn-warning"] $ text "Saving PB",
           onResult (\newS -> handle $ Crud (NewSaved newS)) (handle <<< Crud <<< NewSaveFailed)
         ],
       _Errored $ with \err _ -> mconcat [
           ui $ H.button [H.classA "btn btn-danger"] $ text "Failed!",
           ui $ H.div [H.classA "alert alert-danger"] $ text ("Photobooth not saved: " <> message err)
         ]
      ]

listPhotobooths :: forall eff obj. (PhotoboothsCommand -> Eff (RefDomTimer eff) Unit) -> Maybe Int
                   -> AppUI (RefDomTimer eff) {collection :: (AsyncModel (RefDomTimer eff) (Array Photobooth)), editing :: Maybe {saving :: AsyncModel (RefDomTimer eff) Photobooth | obj}} 
listPhotobooths handle selInd = mconcat [
  _collection $ with \pbs h -> mconcat [
       _Initial $ (exec $ handle $ Crud LoadAll) 
               <> (ui $ H.tr [] $ H.td [] $ text "Nothing loaded yet"),
       _Busy $ mconcat [ ui $ H.tr [] $ H.td [] $ text "Loading photobooths"
                       , onResult (handle <<< Crud <<< Loaded) (handle <<< Crud <<< LoadingFailed) ],
       _Done $ foreach (showPB handle selInd),
       _Errored $ with \err _ -> ui $ H.tr [] $ H.td [] $ text ("Photobooths failed to load: " <> message err)],
  (_editing <<< _Just <<< _saving <<< _Busy) $ onResult (\edited -> handle $ Crud (EditSaved edited)) (handle <<< Crud <<< EditSaveFailed),
  (_editing <<< _Just <<< _saving <<< _Errored) $ with \err h -> mconcat [ ui $ H.td [] mempty 
                                                                         , ui $ H.td [] mempty
                                                                         , ui $ H.td [] mempty
                                                                         , ui $ H.div [H.classA "alert alert-danger"] $ text ("Photobooth edit failed to get saved: " ++ message err) 
                                                                         ]]

showPB :: forall eff. (PhotoboothsCommand -> Eff (dom :: DOM | eff) Unit) -> Maybe Int -> Int -> AppUI (dom :: DOM | eff) Photobooth
showPB handle (Just selInd) i | i == selInd = with \(Photobooth pb) h -> 
  withView (H.tr []) $ mconcat [
    ui $ H.td [] $ text pb.computername,
    withView (H.td []) $ (_Photobooth <<< _alias) $ textField [H.classA "form-control"],
    withView (H.td []) $ (_Photobooth <<< _defaultprofile) $ textField [H.classA "form-control"],
    ui $ H.td [] $ mconcat [
      H.button [H.classA "btn btn-warning", H.onClick \_ -> handle $ Crud CancelEdit] $ text "Cancel",
      H.button [H.classA "btn btn-success", H.onClick \_ -> handle $ Crud SaveEdit] $ text "Save"
      ]
  ]
showPB handle a i = with \(Photobooth pb) h -> 
  ui $ H.tr [] $ mconcat [
    H.td [] $ text pb.computername,
    H.td [] $ text pb.alias,
    H.td [] $ text pb.defaultprofile,
    maybe (H.td [] $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle (Crud $ StartEdit i)] $ text "Edit") (const mempty) a,
    H.td [] $ H.button [H.classA "btn btn-primary", H.onClick \_ -> handle (ToEvents pb.computername)] $ text "Zie events"
  ]

------ AFF -------------

loadPbs :: forall eff. Aff (ajax :: AJAX | eff) (Array Photobooth)
loadPbs = execEndpoint getPhotobooths unit unit

saveNewPb :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Photobooth
saveNewPb pb = execEndpoint postPhotobooths unit pb

updatePB :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Photobooth
updatePB pb = execEndpoint putPhotobooths unit pb
