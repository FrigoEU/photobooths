module Endpoint.Client where

import Data.Either
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM.File.Types (File, Blob)
import Data.Foreign.Generic (defaultOptions, readJSONGeneric, toJSONGeneric)
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Serializable (class Serializable, serialize)
import Debug.Trace (trace)
import Global (encodeURI)
import Network.HTTP.Affjax (AJAX, affjax)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude (return, (<>), ($), (>>>), (>>=))
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
execEndpoint_ s (Endpoint {method: GET, url: u}) a b = 
  affjax opts >>= _.response >>> parseOrThrow
    where url = encodeURI $ s <> u <> "?params=" <> serialize a
          opts = { method: Left GET
                 , url:  url
                 , headers: [ContentType applicationJSON]
                 , content: (Nothing :: Maybe String)
                 , username: Nothing
                 , password: Nothing
                 , withCredentials: false}
execEndpoint_ s (Endpoint {method: method, url: u}) a b = 
  affjax opts >>= _.response >>> parseOrThrow
    where opts = { method: Left method
                 , url: encodeURI $ s <> u <> "?params=" <> serialize a
                 , headers: [ContentType applicationJSON]
                 , content: (Just $ toJSONGeneric defaultOptions b) :: Maybe String
                 , username: Nothing
                 , password: Nothing
                 , withCredentials: false}

-- relative path
execEndpoint :: forall eff a b c. (Serializable a, Generic b, Generic c) =>
                 Endpoint a b c -> a -> b -> Aff (ajax :: AJAX | eff) c
execEndpoint = execEndpoint_ ""

execFileUploadEndpoint :: forall eff a b . (Serializable a, Generic b) =>
                             FileUploadEndpoint a b -> File -> a -> Aff (ajax :: AJAX | eff) b
execFileUploadEndpoint (FileUploadEndpoint {url: u}) file a = affjax opts >>= _.response >>> parseOrThrow
  where opts = { method: Left POST
               , url: encodeURI $ u <> "?params=" <> serialize a
               , headers: []
               , content: Just $ fileToBlob file
               , username: Nothing
               , password: Nothing
               , withCredentials: false}

-- Atm affjax does nat define File as Requestable. I could also make a PR for Affjax, but this is easier...
fileToBlob :: File -> Blob
fileToBlob = unsafeCoerce

parseOrThrow :: forall eff a. (Generic a) => String -> Aff eff a
parseOrThrow s = 
  either (\e -> throwError $ error $ "Failed to parse: " <> s) return (readJSONGeneric defaultOptions s)
