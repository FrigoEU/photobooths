module Klikhut.Async where

import Prelude (Unit(), unit, const)
import Data.Lens (prism', PrismP())
import Data.Maybe (Maybe(..))
import OpticUI.Components.Async (Async())
import Control.Monad.Eff.Exception (Error())

data AsyncModel eff a  = Initial
                       | Busy (Async eff a)
                       | Errored Error
                       | Done a

_Initial :: forall eff a. PrismP (AsyncModel eff a) Unit
_Initial = prism' (const Initial) l
  where l (Initial) = Just unit
        l _ = Nothing

_Busy :: forall eff a. PrismP (AsyncModel eff a) (Async eff a)
_Busy = prism' Busy l
  where l (Busy x) = Just x
        l _ = Nothing

_Errored :: forall eff a. PrismP (AsyncModel eff a) Error
_Errored = prism' Errored l 
  where l (Errored x) = Just x
        l _ = Nothing

_Done :: forall eff a. PrismP (AsyncModel eff a) a
_Done = prism' Done l
  where l (Done x) = Just x
        l _ = Nothing
