module App.FS where
  
import Prelude (Unit, show, (<>), unit, return, (>>=), bind, not)
import Data.Traversable (traverse)
import Control.Monad.Aff (Aff)
import Node.Buffer (Buffer, BUFFER)
import Node.FS (FS)
import Node.FS.Aff (stat, readdir, mkdir, writeFile, exists, rmdir, readFile, unlink)
import Node.FS.Stats (isDirectory)
import Node.Path (concat, FilePath)

-- mkdir throws if the directory already exists, safeMkdir doesn't
safeMkdir :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
safeMkdir f = exists f >>= (\doesExist -> if (not doesExist) then mkdir f else return unit)

-- rmdir doesn't remove folders when they're not empty, rmdirRecur does
rmdirRecur :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
rmdirRecur d = do 
  exist <- exists d
  if (not exist) then return unit
                 else do filesToDelete <- readdir d
                         traverse (\f -> do
                                      let path = concat [d, f]
                                      s <- stat path
                                      if isDirectory s then rmdirRecur path else unlink path ) filesToDelete
                         rmdir d
  
overWriteFile :: forall eff. FilePath -> Buffer -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
overWriteFile f b = do
  exist <- exists f
  if (exist) then unlink f
             else return unit
  writeFile f b 

copyDir :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyDir from to = do
  rmdirRecur to
  filesToCopy <- readdir from
  mkdir to
  traverse (\f -> readFile (concat [from, f]) >>= writeFile (concat [to, f])) filesToCopy
  return unit

mkEventDir :: Int -> FilePath
mkEventDir i =  concat ["clientprofiles", "event_" <> show i]

mkTempEventDir :: Int -> FilePath
mkTempEventDir i = concat ["clientprofiles", "tmp_event_" <> show i]

defaultDir :: FilePath
defaultDir = concat ["clientprofiles", "default"]
