module Endpoint.Client where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error, message)

import Data.Either (Either(..), either)
import Data.String.Regex (Regex())
import Data.Maybe(Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foreign.Generic (defaultOptions, readJSONGeneric, toJSONGeneric)
import Data.Generic (Generic)

import Network.HTTP.Affjax (get, post, put, AJAX(), URL(), affjax)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.MimeType.Common (applicationJSON, imageJPEG)
import Network.HTTP.Method (Method(..))

import DOM.File.Types (File(), Blob())

import Node.Express.Types (RoutePattern)

import Unsafe.Coerce

-----------------------------------------

data Endpoint a b c = Endpoint {
  method :: Method,
  serverUrl :: String,
  mkClientUrl :: a -> URL
}

data FileEndpoint a b = FileEndpoint {
  serverUrl :: String,
  mkClientUrl :: a -> URL
  }

execEndpoint :: forall eff a b c. (Generic b, Generic c) =>
                  Endpoint a b c -> a -> b -> Aff (ajax :: AJAX | eff) c
execEndpoint (Endpoint {method: method, mkClientUrl: f}) a b = 
  affjax opts >>= _.response >>> parseOrThrow
    where opts = { method: method
                 , url: f a
                 , headers: [ContentType applicationJSON]
                 , content: (Just $ toJSONGeneric defaultOptions b) :: Maybe String
                 , username: Nothing
                 , password: Nothing}

sendJpeg :: forall eff a b . (Generic b) =>
                             FileEndpoint a b -> File -> a -> Aff (ajax :: AJAX | eff) b
sendJpeg (FileEndpoint {mkClientUrl: f}) file a = affjax opts >>= _.response >>> parseOrThrow
  where opts = { method: POST
               , url: f a
               , headers: [ContentType imageJPEG]
               , content: Just $ fileToBlob file
               , username: Nothing
               , password: Nothing}

-- Atm affjax does nat define File as Requestable. I could also make a PR for Affjax, but this is easier...
fileToBlob :: File -> Blob
fileToBlob = unsafeCoerce

parseOrThrow :: forall eff a. (Generic a) => String -> Aff eff a
parseOrThrow a = 
  either (\e -> throwError $ error $ show e) return (readJSONGeneric defaultOptions a)
