module Node.OS where

import Control.Monad.Eff

foreign import data OS :: !
foreign import hostname :: forall eff. Eff (os :: OS | eff) String
