module Server.Core (
  makeApp, listen, hostEndpoint, hostFile, hostFileEndpoint, Input(), Handler(), App(), EXPRESS()
) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (runAff, Aff())
import Control.Monad.Eff.Exception (message, error)
import Control.Monad.Error.Class (throwError)

import Data.Function (Fn2(), mkFn2)
import Data.StrMap (StrMap())
import Data.Either (either)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Core (Json())

import Network.HTTP.Method (Method(..))

import Node.Buffer (Buffer())

import App.Endpoint

foreign import data App :: *
foreign import data EXPRESS :: !
foreign import data Request :: *
foreign import data Response :: *
foreign import data Middleware :: *

foreign import makeApp :: forall eff. Eff (express :: EXPRESS | eff) App
foreign import listen :: forall eff. App -> Int -> Eff (express :: EXPRESS | eff) Unit
foreign import get :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import post :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import put :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import sendStr  :: forall eff. Response -> String -> Eff (express :: EXPRESS | eff) Unit
foreign import sendFile :: forall eff. Response -> String -> Eff (express :: EXPRESS | eff) Unit
foreign import jsonParser :: Middleware
foreign import jpegParser :: Middleware

type Handler eff a b = Input a -> Aff eff b

type Input a = { url :: String
               , body :: a
               , params :: StrMap String
               , path :: String 
               , query :: StrMap String
               , headers :: StrMap String
               }

hostEndpoint :: forall a b c eff. (EncodeJson b, DecodeJson b, EncodeJson c, DecodeJson c) =>
                  App -> Endpoint a b c -> Handler (express :: EXPRESS, console :: CONSOLE | eff) b c
                  -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostEndpoint app (Endpoint {method: GET, serverUrl: url}) f = get app url jsonParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint GET on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ printJson $ encodeJson a) 
                             (parseBodyOrThrow (convertJson req) >>= f)
hostEndpoint app (Endpoint {method: POST, serverUrl: url}) f = post app url jsonParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint POST on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ printJson $ encodeJson a) 
                             (parseBodyOrThrow (convertJson req) >>= f)
hostEndpoint app (Endpoint {method: PUT, serverUrl: url}) f = put app url jsonParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint PUT on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ printJson $ encodeJson a) 
                             (parseBodyOrThrow (convertJson req) >>= f)

hostFileEndpoint :: forall eff a b. (EncodeJson b, DecodeJson b) =>
                      App -> FileEndpoint a b 
                      -> Handler (express :: EXPRESS, console :: CONSOLE | eff) Buffer b  
                      -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFileEndpoint app (FileEndpoint {serverUrl: url}) f = post app url jpegParser handler
  where handler req res = runAff (\err -> do
                                     log $ "Failed hostFileEndpoint on " <> url <> message err
                                     sendStr res $ message err)
                                 (\a -> sendStr res $ printJson $ encodeJson a) 
                                 (f $ convertBuffer req)


parseBodyOrThrow :: forall a eff. (DecodeJson a) => Input Json -> Aff eff (Input a)
parseBodyOrThrow a = either (\str -> throwError $ error ("failed to parse input: " <> str))
                            (\p -> return $ a { body = p})
                            (decodeJson a.body)

hostFile :: forall eff. 
              App -> String -> (Input Unit -> Aff (express :: EXPRESS, console :: CONSOLE | eff) String) 
              -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFile app url f = get app url jsonParser handler
  where 
    handler req res = runAff (\err -> do
                               log $ "Failed hostFile " <> message err
                               sendStr res $ message err)
                             (\a -> sendFile res a)
                             (parseBodyOrThrow (convertJson req) >>= f)

convertJson :: Request -> Input Json
convertJson = mkConvert {url: _, body: _, params: _, path: _, query: _, headers: _}

convertBuffer :: Request -> Input Buffer
convertBuffer = mkBufferConvert {url: _, body: _, params: _, path: _, query: _, headers: _}

foreign import mkConvert :: 
  forall a. (String -> a -> StrMap String -> String -> StrMap String -> StrMap String -> Input a) 
            -> Request -> Input a
foreign import mkBufferConvert :: 
  (String -> Buffer -> StrMap String -> String -> StrMap String -> StrMap String -> Input Buffer) 
            -> Request -> Input Buffer
