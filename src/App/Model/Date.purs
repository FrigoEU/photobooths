module App.Model.Date where

import Data.Date (Date())

foreign import iso8601 :: Date -> String
