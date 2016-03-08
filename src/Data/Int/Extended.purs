module Data.Int.Extended where

import Data.Maybe

foreign import pInt :: forall a. Maybe a -> (a -> Maybe a) -> String -> Maybe Int

safeParseInt :: String -> Maybe Int
safeParseInt str = pInt Nothing Just str
