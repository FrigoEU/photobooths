module Test.Main where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff.Console
import qualified Test.Klikhut.SQL as SQL
import qualified Test.Klikhut.Model.Photobooth as PB
import Data.Foldable (foldl)

import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

main = run [consoleReporter] $ foldl (*>) (return unit) 
         [SQL.main, PB.main]
