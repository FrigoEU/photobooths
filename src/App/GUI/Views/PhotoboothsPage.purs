module App.GUI.Views.PhotoboothsPage (photoboothsPage) where

import OpticUI.Markup.HTML as H
import OpticUI.Markup as H
import OpticUI.Components (textField)
import OpticUI (with, withView, ui, text, foreach)
import OpticUI.Components.Async (onResult)

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff (Eff())

import Data.Lens (view)
import Data.Lens.Common (_Just)
import Data.Foldable (mconcat)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.StrMap (StrMap(), lookup)

import Prelude 
import DOM (DOM())
import Network.HTTP.Affjax (AJAX())

import App.Model.Photobooth (Photobooth(Photobooth), sortPhotobooths, _Photobooth)
import App.Model.Async (AsyncModel(..), _Busy, _Done, _Initial) 
import App.Model.Profile (Profiles()) 
import App.GUI.Router (Nav()) 
import App.GUI.Types (AppUI(), RefDomTimer(), RefDom(), AjaxRefDomTimer()) 
import App.GUI.State
import App.GUI.Components.Exec (exec) 
import App.GUI.Components.Select (select) 
import App.GUI.Components.CrudButtons
import App.GUI.Components.Markup
import App.GUI.Views.Crud 
import App.GUI.Views.Profiles (loadProfiles) 
import App.Endpoint  


data PhotoboothsCommand = Crud (CrudCommand Photobooth)
                        | ToEvents String String
                        | ToStatistics String String

photoboothsPage :: forall eff. Nav (AjaxRefDomTimer eff) ->
                               AppUI (AjaxRefDomTimer eff) { collection :: AsyncModel (AjaxRefDomTimer eff) (Array Photobooth)
                                                           , profiles :: AsyncModel (AjaxRefDomTimer eff) Profiles
                                                           , new :: { model :: {id :: Maybe Int, computername :: String, alias :: String, defaultprofile :: String}
                                                                    , state :: AsyncModel (AjaxRefDomTimer eff) Photobooth}
                                                           , editing :: Maybe {index :: Int, previous :: Photobooth, saving :: AsyncModel (AjaxRefDomTimer eff) Photobooth}
                                                           , deleting :: Maybe {index :: Int, saving :: AsyncModel (AjaxRefDomTimer eff) Unit}}
photoboothsPage goto = with $ \s h ->
  let impls = {loadAll: loadPbs, delete: deletePB, saveNew: saveNewPb, saveEdit: updatePB, initial: return {id: Nothing, computername: "", alias: "", defaultprofile: ""}, constr: Photobooth } 
      handle (Crud a) = crudHandler s h impls a
      handle (ToEvents name alias) = goto (EventsPage name alias)
      handle (ToStatistics name alias) = goto (StatisticsPage name alias)
   in ui (pageTitle $ H.text "Photobooths")
   <> (withView crudTable $ mconcat [ ui $ tableHeader [] ["Name", "Alias", "Default Profile", "Actions", "Link", "Delete"]
                                    , _new (makeNewPb handle)
                                    , _collectionEditingD (listPhotobooths handle (view _editing s >>= \ed -> return ed.index) (view _deleting s >>= \d -> return d.index) (view (_profiles <<< _Done) s))
                                    , _profiles loadProfiles
                                    ])
                                    
------ LIST --------------

listPhotobooths :: forall eff obj. (PhotoboothsCommand -> Eff (RefDomTimer eff) Unit) 
                                   -> Maybe Int 
                                   -> Maybe Int
                                   -> StrMap (Array String) 
                                   -> AppUI (RefDomTimer eff) { collection :: (AsyncModel (RefDomTimer eff) (Array Photobooth))
                                                              , editing :: Maybe {index :: Int, saving :: AsyncModel (RefDomTimer eff) Photobooth | obj}
                                                              , deleting :: Maybe {index :: Int, saving :: AsyncModel (RefDomTimer eff) Unit}} 
listPhotobooths handle selInd delInd profiles = mconcat [
  with (\s _ -> _collection $ with (showColl $ view _editing s)),
  (_editing  <<< _Just <<< _saving <<< _Busy) $ onResult (handle <<< Crud <<< EditSaved) (handle <<< Crud <<< EditSaveFailed),
  (_deleting <<< _Just <<< _saving <<< _Busy) $ onResult (handle <<< (const $ Crud DeleteDone)) (handle <<< Crud <<< DeleteFailed)
  ]
    where
      showColl _ Initial       h = (ui $ H.tr [] $ H.td [] $ text "Nothing loaded yet")
                                <> _Initial (exec $ handle $ Crud LoadAll) 
      showColl _ (Busy _)      h = ui (H.tr [] $ H.td [] $ text "Loading photobooths")
                                <> _Busy (onResult (handle <<< Crud <<< Loaded) (handle <<< Crud <<< LoadingFailed))
      showColl e (Done _)      h = _Done $ foreach (showPB handle selInd delInd profiles e)
      showColl _ (Errored err) h = ui $ H.tr [] $ H.td [] $ text ("Photobooths failed to load: " <> message err)
 
showPB :: forall eff obj a. (PhotoboothsCommand -> Eff (dom :: DOM | eff) Unit) 
                            -> Maybe Int 
                            -> Maybe Int 
                            -> StrMap (Array String) 
                            -> Maybe { index :: Int, saving :: AsyncModel (dom :: DOM | eff) a | obj } 
                            -> Int 
                            -> AppUI (dom :: DOM | eff) Photobooth
showPB handle (Just selInd) _     profiles editing i | i == selInd = with \(Photobooth pb) h -> 
  rowUI [ ui $ text pb.computername
        , (_Photobooth <<< _alias) $ textField [H.classA "form-control"]
        , (_Photobooth <<< _defaultprofile) $ select (fromMaybe [] $ lookup pb.computername profiles) id [H.classA "form-control"]
        , editButton (handle <<< Crud) i editing
        , ui $ linkButtons handle pb.computername pb.alias 
        ]
showPB handle selInd       delInd _        editing i = with \(p@(Photobooth pb)) h -> 
  rowUI $ ui <$> [ text pb.computername
                 , text pb.alias
                 , text pb.defaultprofile
                 , maybe (H.button [H.classA "btn btn-action", H.onClick \_ -> handle (Crud $ StartEdit i)] $ text "Edit") (const $ text "") selInd
                 , linkButtons handle pb.computername pb.alias
                 , case delInd of 
                        (Just ix) | i == ix -> (H.button [H.classA "btn btn-danger", H.onClick \_ -> handle $ Crud $ Delete p] $ text "Sure?")
                                               <> (H.button [H.classA "btn btn-primary", H.onClick \_ -> handle $ Crud $ CancelDelete] $ text "Cancel!")
                        (Just _)            -> text ""
                        Nothing             -> H.button [H.classA "btn btn-danger", H.onClick \_ -> handle $ Crud $ StartDelete i] $ text "Delete"

                 ]

linkButtons :: forall eff. (PhotoboothsCommand -> Eff eff Unit) -> String -> String -> H.Markup 
linkButtons handle cn alias = H.button [H.classA "btn btn-primary", H.onClick \_ -> handle (ToEvents cn alias)] (text "Zie events")
                           <> H.button [H.classA "btn btn-primary", H.onClick \_ -> handle (ToStatistics cn alias)] (text "Zie statistieken")

----------- NEW ---------------------

makeNewPb :: forall eff. (PhotoboothsCommand -> Eff (RefDom eff) Unit) 
                         -> AppUI (RefDom eff) { model :: {id :: Maybe Int, computername :: String, alias :: String, defaultprofile :: String}
                                                , state :: AsyncModel (RefDom eff) Photobooth}
makeNewPb handle = with c 
  where 
    c model h = rowUI [ (_model <<< _computername) $ textField [H.classA "form-control"]
                      , (_model <<< _alias)        $ textField [H.classA "form-control"]
                      , ui emptyTd
                      , _state                     $ (newButton (handle <<< Crud))
                      , ui emptyTd
                      ]


------ AFF -------------

loadPbs :: forall eff. Aff (ajax :: AJAX | eff) (Array Photobooth)
loadPbs = execEndpoint getPhotobooths unit unit >>= sortPhotobooths >>> return

saveNewPb :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Photobooth
saveNewPb pb = execEndpoint postPhotobooths unit pb

updatePB :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Photobooth
updatePB pb = execEndpoint putPhotobooths unit pb

deletePB :: forall eff. Photobooth -> Aff (ajax :: AJAX | eff) Unit
deletePB (Photobooth pb) = execEndpoint deletePhotobooth pb.computername unit
