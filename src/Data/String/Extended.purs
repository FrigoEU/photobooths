module Data.String.Extended where

import Prelude ((==))
import Data.String (length, take)

startsWith :: String -> String -> Boolean
startsWith big small = take (length small) big == small
    
