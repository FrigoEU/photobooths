module App.MakeDB where

import App.DB (networkingConnectionInfo, makeDB)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (log)
import Database.AnyDB (withConnection)
import Prelude ((<<<), show)



main = runAff (log <<< show) (\_ -> log "DB made") (withConnection networkingConnectionInfo makeDB)
