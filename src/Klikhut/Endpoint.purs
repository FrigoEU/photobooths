module Klikhut.Endpoint where

import Prelude

import Network.HTTP.Affjax (get, post, put, AJAX(), URL(), affjax)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.MimeType.Common (applicationJSON)
import Network.HTTP.Method (Method(..))
import Control.Monad.Aff (Aff())
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson, DecodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error, message)
import Data.Either (Either(..), either)
import Node.Express.Types (RoutePattern)
import Data.String.Regex (Regex())
import Data.Maybe(Maybe(..))

import Klikhut.Model.Photobooth as PB

data Endpoint a b c = Endpoint {
  method :: Method,
  serverUrl :: String,
  mkClientUrl :: a -> URL
}

execEndpoint :: forall eff a b c. (EncodeJson b, DecodeJson b, EncodeJson c, DecodeJson c) =>
                  Endpoint a b c -> a -> b -> Aff (ajax :: AJAX | eff) c
execEndpoint (Endpoint {method: method, mkClientUrl: f}) a b = 
  affjax opts >>= _.response >>> parseOrThrow
    where opts = { method: method
                 , url: f a
                 , headers: [ContentType applicationJSON]
                 , content: (Just $ printJson $ encodeJson b) :: Maybe String
                 , username: Nothing
                 , password: Nothing}


getPhotobooths :: forall a. Endpoint a Unit (Array PB.Photobooth)
getPhotobooths = Endpoint { method: GET, serverUrl: "/api/photobooths"
                          , mkClientUrl: const "/api/photobooths"} 

postPhotobooths :: forall a. Endpoint a PB.Photobooth Unit
postPhotobooths = Endpoint { method: POST, serverUrl: "/api/photobooths"
                           , mkClientUrl: const "/api/photobooths"}

putPhotobooths :: forall a. Endpoint a PB.Photobooth Unit
putPhotobooths = Endpoint { method: PUT, serverUrl: "/api/photobooths"
                          , mkClientUrl: const "/api/photobooths"}

parseOrThrow :: forall eff a. (DecodeJson a) => String -> Aff eff a
parseOrThrow a = 
  either throwStr (\json -> either throwStr return $ decodeJson json) (jsonParser a)
    where throwStr str = throwError $ error str
