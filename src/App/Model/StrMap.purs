module App.Model.StrMap where

import Prelude

import Data.StrMap (StrMap(), keys, insert, foldMap, empty, fold)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Monoid (mempty)
import Data.Array (snoc)

foreign import _collect :: forall a b . (String -> a -> b) -> StrMap a -> Array b

toArray :: forall a. StrMap a -> Array (Tuple String a)
toArray = _collect Tuple

fromArray :: forall a. Array (Tuple String a) -> StrMap a
fromArray = foldl (\acc (Tuple str a) -> insert str a acc) empty
