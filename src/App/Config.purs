module App.Config where

import Control.Monad.Eff (Eff)
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Generic (class Generic, gShow)
import Node.FS (FS)
import Node.Path (concat)
import Node.Process (PROCESS, cwd)
import Prelude (class Show, (<$>), bind, return, ($))

data WorkerConfig = WorkerConfig { webServiceHost :: String
                                 , photoProgramExe :: String
                                 , photoProgramFullPath :: String }

derive instance genericWorkerConfig :: Generic WorkerConfig
instance showWorkerConfig :: Show WorkerConfig where show = gShow

instance isForeignWorkerConfig :: IsForeign WorkerConfig where
  read f = do 
    webServiceHost <- readProp "webservice" f 
    photoProgramFullPath <- readProp "photoprogramfullpath" f
    photoProgramExe <- readProp "photoprogram" f
    return $ WorkerConfig {webServiceHost, photoProgramFullPath, photoProgramExe}

{-- improveWorkerConfigForeignError :: F WorkerConfig -> F WorkerConfig --}
{-- improveWorkerConfigForeignError (Left (ErrorAtProperty "webservice" a)) = Left $ ErrorAtProperty --} 
{--   "'webservice' field not found or incorrect format in configuration file" a --}
{-- improveWorkerConfigForeignError (Left (ErrorAtProperty "photoprogramfullpath" a)) = Left $ ErrorAtProperty --}
{--   "'photoprogram' field not found or incorrect format in configuration file" a --}
{-- improveWorkerConfigForeignError a = a --}

foreign import requireConfigFile :: forall eff. String -> Eff (fs :: FS | eff) Foreign

readConfigFile :: forall eff. Eff (fs :: FS, process :: PROCESS | eff) (F WorkerConfig)
readConfigFile = do 
  c <- cwd
  read <$> requireConfigFile (concat [c, "config.json"])

{-- test = readConfigFile >>= show >>> log --}
