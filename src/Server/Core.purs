module Server.Core (
  makeApp, listen, hostEndpoint, hostFile, hostFileEndpoint, Input(), Handler(), App(), EXPRESS()
) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (runAff, Aff())
import Control.Monad.Eff.Exception (message, error)
import Control.Monad.Error.Class (throwError)

import Data.StrMap (StrMap())
import Data.Either (either)
import Data.Foreign.Generic (readJSONGeneric, toJSONGeneric, defaultOptions)
import Data.Generic (Generic)

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
foreign import noParser :: Middleware
foreign import rawParser :: Middleware

type Handler eff a b = Input a -> Aff eff b

type Input a = { url :: String
               , body :: a
               , params :: StrMap String
               , path :: String 
               , query :: StrMap String
               , headers :: StrMap String
               }

hostEndpoint :: forall a b c eff. (Generic b, Generic c) =>
                  App -> Endpoint a b c -> Handler (express :: EXPRESS, console :: CONSOLE | eff) b c
                  -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostEndpoint app (Endpoint {method: GET, serverUrl: url}) f = get app url noParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint GET on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ toJSONGeneric defaultOptions a) 
                             (parseBodyOrThrow (convert req) >>= f)
hostEndpoint app (Endpoint {method: POST, serverUrl: url}) f = post app url rawParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint POST on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ toJSONGeneric defaultOptions a) 
                             (parseBodyOrThrow (convert req) >>= f)
hostEndpoint app (Endpoint {method: PUT, serverUrl: url}) f = put app url rawParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint PUT on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ toJSONGeneric defaultOptions a) 
                             (parseBodyOrThrow (convert req) >>= f)

hostFileEndpoint :: forall eff a b. (Generic b) =>
                      App -> FileEndpoint a b 
                      -> Handler (express :: EXPRESS, console :: CONSOLE | eff) Buffer b  
                      -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFileEndpoint app (FileEndpoint {serverUrl: url}) f = post app url jpegParser handler
  where handler req res = runAff (\err -> do
                                     log $ "Failed hostFileEndpoint on " <> url <> message err
                                     sendStr res $ message err)
                                 (\a -> sendStr res $ toJSONGeneric defaultOptions a) 
                                 (f $ convertBuffer req)


parseBodyOrThrow :: forall a eff. (Generic a) => Input String -> Aff eff (Input a)
parseBodyOrThrow a = either (\err -> throwError $ error $ show err)
                            (\p -> return $ a { body = p})
                            (readJSONGeneric defaultOptions a.body)

hostFile :: forall eff. 
              App -> String -> (Input Unit -> Aff (express :: EXPRESS, console :: CONSOLE | eff) String) 
              -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFile app url f = get app url noParser handler
  where 
    handler req res = runAff (\err -> do
                               log $ "Failed hostFile " <> message err
                               sendStr res $ message err)
                             (\a -> sendFile res a)
                             (parseBodyOrThrow (convert req) >>= f)

convert :: Request -> Input String
convert = mkConvert {url: _, body: _, params: _, path: _, query: _, headers: _}
                    (toJSONGeneric defaultOptions unit)

convertBuffer :: Request -> Input Buffer
convertBuffer = mkBufferConvert {url: _, body: _, params: _, path: _, query: _, headers: _}

foreign import mkConvert :: 
  forall a. (String -> a -> StrMap String -> String -> StrMap String -> StrMap String -> Input a) 
            -> String -> Request -> Input a
foreign import mkBufferConvert :: 
  (String -> Buffer -> StrMap String -> String -> StrMap String -> StrMap String -> Input Buffer) 
            -> Request -> Input Buffer
