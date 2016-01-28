module Endpoint.Client where

import Prelude (return, show, ($), (>>>), (>>=), (<>))

import Control.Monad.Aff (Aff())
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)

import Data.Either (either)
import Data.Maybe(Maybe(..))
import Data.Foreign.Generic (defaultOptions, readJSONGeneric, toJSONGeneric)
import Data.Generic (class Generic)
import Data.Serializable (class Serializable, serialize)

import Network.HTTP.Affjax (AJAX, affjax)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.MimeType.Common (applicationJSON)
import Network.HTTP.Method (Method(..))

import DOM.File.Types (File(), Blob())

import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------

data Endpoint a b c = Endpoint {
  method :: Method,
  url :: String
}

data FileUploadEndpoint a b = FileUploadEndpoint {
  url :: String
}

execEndpoint_ :: forall eff a b c. (Serializable a, Generic b, Generic c) =>
                  String -> Endpoint a b c -> a -> b -> Aff (ajax :: AJAX | eff) c
execEndpoint_ s (Endpoint {method: method, url: u}) a b = 
  affjax opts >>= _.response >>> parseOrThrow
    where opts = { method: method
                 , url: s <> u <> "?params=" <> serialize a
                 , headers: [ContentType applicationJSON]
                 , content: (Just $ toJSONGeneric defaultOptions b) :: Maybe String
                 , username: Nothing
                 , password: Nothing}

-- relative path
execEndpoint :: forall eff a b c. (Serializable a, Generic b, Generic c) =>
                 Endpoint a b c -> a -> b -> Aff (ajax :: AJAX | eff) c
execEndpoint = execEndpoint_ ""

execFileUploadEndpoint :: forall eff a b . (Serializable a, Generic b) =>
                             FileUploadEndpoint a b -> File -> a -> Aff (ajax :: AJAX | eff) b
execFileUploadEndpoint (FileUploadEndpoint {url: u}) file a = affjax opts >>= _.response >>> parseOrThrow
  where opts = { method: POST
               , url: u <> "?params=" <> serialize a
               , headers: []
               , content: Just $ fileToBlob file
               , username: Nothing
               , password: Nothing}

-- Atm affjax does nat define File as Requestable. I could also make a PR for Affjax, but this is easier...
fileToBlob :: File -> Blob
fileToBlob = unsafeCoerce

parseOrThrow :: forall eff a. (Generic a) => String -> Aff eff a
parseOrThrow a = 
  either (\e -> throwError $ error $ show e) return (readJSONGeneric defaultOptions a)
