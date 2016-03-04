module App.Config where

import Prelude (class Show, (<$>), bind, ($), return, (<>))

import Node.FS (FS)
import Control.Monad.Eff (Eff)
import Data.Foreign (F, Foreign, ForeignError(ErrorAtProperty))
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Either (Either(Left))
import Node.Process (PROCESS, cwd)
import Node.Path (concat)

data WorkerConfig = WorkerConfig { webServiceHost :: String
                                 , photoProgramPath :: String }

instance showWorkerConfig :: Show WorkerConfig where
  show (WorkerConfig {webServiceHost, photoProgramPath}) = 
    "WorkerConfig: webServiceHost: " <> webServiceHost <> ", photoProgramPath: " <> photoProgramPath

instance isForeignWorkerConfig :: IsForeign WorkerConfig where
  read f = do 
    webServiceHost <- readProp "webservice" f 
    photoProgramPath <- readProp "photoprogram" f
    return $ WorkerConfig {webServiceHost, photoProgramPath}

improveWorkerConfigForeignError :: F WorkerConfig -> F WorkerConfig
improveWorkerConfigForeignError (Left (ErrorAtProperty "webservice" a)) = Left $ ErrorAtProperty 
  "'webservice' field not found or incorrect format in configuration file" a
improveWorkerConfigForeignError (Left (ErrorAtProperty "photoprogram" a)) = Left $ ErrorAtProperty
  "'photoprogram' field not found or incorrect format in configuration file" a
improveWorkerConfigForeignError a = a

foreign import requireConfigFile :: forall eff. String -> Eff (fs :: FS | eff) Foreign

readConfigFile :: forall eff. Eff (fs :: FS, process :: PROCESS | eff) (F WorkerConfig)
readConfigFile = do 
  c <- cwd
  read <$> requireConfigFile (concat [c, "config.json"])

{-- test = readConfigFile >>= show >>> log --}
