module Test.Main where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff.Console
import qualified Test.App.SQL as SQL
import qualified Test.App.Model.Photobooth as PB
import qualified Test.App.Model.Event as E
import Data.Foldable (foldl)

import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] $ foldl (*>) (return unit) 
         [SQL.main, PB.main, E.main]
