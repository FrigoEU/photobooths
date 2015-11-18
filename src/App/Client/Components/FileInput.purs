module App.Client.Components.FileInput (fileInput, onFileInput, name, accept) where

import Prelude
import Control.Monad.Eff (Eff())

import OpticUI (with, ui, runHandler, UI())
import OpticUI.Markup (Event(), Prop(), handle, attr, Markup())
import OpticUI.Markup.HTML as H

import DOM (DOM())
import DOM.File.Types (File(), Blob())
import Data.Maybe
import Data.Monoid (mempty)
import Data.Array ((!!))
import Data.Foreign.Class (IsForeign)
import Unsafe.Coerce

import App.Client.Types

{-- instance myFileIsForeign :: IsForeign MyFile where --}
{--   read = unsafeCoerce --}

{-- newtype MyFile = MyFile File --}

{-- toFile :: MyFile -> File --}
{-- toFile = unsafeCoerce --}

-- Does not allow SETTING a file, only allows SETTING it to Nothing
fileInput :: forall eff. Array Prop -> UI eff Markup (Maybe File) (Maybe File)
fileInput ps = with $ \s h ->
  let props = [H.typeA "file", onFileInput $ \_ f -> runHandler h f] 
               <> (if isNothing s then [H.valueA ""] else mempty)
   in ui $ H.input_ (ps <> props) 

onFileInput :: forall eff r. (Event r -> Maybe File -> Eff eff Unit) -> Prop
onFileInput h = handle "change" $ \e -> h e (firstFile e)

foreign import name :: File -> String
foreign import firstFileImpl :: forall a r. Maybe a -> (a -> Maybe a) -> Event r -> Maybe File

firstFile :: forall r. Event r -> Maybe File
firstFile = firstFileImpl Nothing Just

accept :: String -> Prop
accept = attr "accept"
