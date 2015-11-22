module App.GUI.Views.Crud where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Exception (message, Error())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())

import OpticUI (runHandler, Handler())
import OpticUI.Components.Async (onResult, async)

import Data.Lens (set, over, view)
import Data.Lens.Common (_Just, _Nothing)
import Data.Array (snoc, (!!), updateAt)
import Data.Maybe (Maybe(..), maybe)

import App.GUI.Types
import App.GUI.State
import App.Model.Async

--------------------------------------------------------

data CrudCommand a = LoadAll
                   | Loaded (Array a)
                   | LoadingFailed Error
                   | SaveNew
                   | NewSaved a
                   | NewSaveFailed Error
                   | StartEdit Int
                   | CancelEdit
                   | SaveEdit
                   | EditSaved a
                   | EditSaveFailed Error

type CrudModel a b obj eff = { collection :: AsyncModel eff (Array a)
                             , new :: { model :: b
                                      , state :: AsyncModel eff a}
                             , editing :: Maybe {index :: Int, previous :: a, saving :: AsyncModel eff a} | obj}

crudHandler :: forall a b obj eff. CrudModel a b obj (ref :: REF | eff)
                                   -> Handler (ref :: REF | eff) (CrudModel a b obj (ref :: REF | eff))
                                   -> { loadAll :: Aff (ref :: REF | eff) (Array a)
                                      , saveNew :: a -> Aff (ref :: REF | eff) a
                                      , saveEdit :: a -> Aff (ref :: REF | eff) a
                                      , initial :: Eff (ref :: REF | eff) b
                                      , constr :: b -> a}
                                   -> CrudCommand a -> Eff (ref :: REF | eff) Unit
crudHandler s h impls comm = handle comm
  where
    updateEditingAndStop replacement = maybe (return unit) 
                                      (\{index: ind, previous: old} -> let updates = set _editing Nothing <<<
                                                                                     over (_collection <<< _Done) (\as -> maybe as id (updateAt ind replacement as))
                                                                        in runHandler h $ updates s)
                                      (view _editing s)
    handle LoadAll = async impls.loadAll >>= \a -> runHandler h (set _collection (Busy a) s)
    handle SaveNew = async (impls.saveNew (impls.constr (view (_new <<< _model) s))) >>= \a -> runHandler h (set (_new <<< _state) (Busy a) s)
    handle (Loaded as) = runHandler h (set _collection (Done as) s)
    handle (LoadingFailed err) = runHandler h (set _collection (Errored err) s)
    handle (NewSaveFailed err) = runHandler h (set (_new <<< _state) (Errored err) s)
    handle (NewSaved new) = do
      init <- impls.initial 
      runHandler h $ (updates init) s
        where
          updates init = over (_collection <<< _Done) (\arr -> snoc arr new) <<< 
                         set (_new <<< _model) init <<< 
                         set (_new <<< _state) (Initial) 
    handle (StartEdit i) = maybe (return unit) 
                                 (\a -> runHandler h (set _editing (Just {index: i, previous: a, saving: Initial}) s)) 
                                 (view (_collection <<< _Done) s !! i) 
    handle CancelEdit = maybe (return unit) 
                              (\{previous: old} -> updateEditingAndStop old) 
                              (view _editing s)
    handle SaveEdit = maybe (return unit) 
                            (\a -> async (impls.saveEdit a) >>= \b -> runHandler h (set (_editing <<< _Just <<< _saving) (Busy b) s))
                            (view _editing s >>= \{index: ind} -> view (_collection <<< _Done) s !! ind)
    handle (EditSaved new) = updateEditingAndStop new
    handle (EditSaveFailed err) = runHandler h $ set (_editing <<< _Just <<< _saving) (Errored err) s
