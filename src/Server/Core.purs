module Server.Core (
  makeApp, listen, hostEndpoint, hostFile, hostFileUploadEndpoint, Input(), Handler(), App(), EXPRESS(),
  hostStatic
) where

import Prelude (Unit, unit, (>>=), ($), (<>), bind, return, show)

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (runAff, Aff())
import Control.Monad.Eff.Exception (message, error, Error)
import Control.Monad.Error.Class (throwError, class MonadError)

import Data.StrMap (StrMap)
import Data.Either (Either(Left), either)
import Data.Foreign.Generic (readJSONGeneric, toJSONGeneric, defaultOptions)
import Data.Generic (class Generic)
import Data.Serializable (class Serializable, deserialize)
import Data.Maybe (Maybe(Nothing, Just), maybe)

import Network.HTTP.Method (Method(..))

import Node.Buffer (Buffer())

import App.Endpoint (Endpoint(Endpoint), FileUploadEndpoint(FileUploadEndpoint))

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
foreign import sendBuffer :: forall eff. Response -> Buffer -> Eff (express :: EXPRESS | eff) Unit
foreign import hostStatic :: forall eff. App -> String -> Eff (express :: EXPRESS | eff) Unit
foreign import jsonParser :: Middleware
foreign import bufferParser :: Middleware
foreign import noParser :: Middleware
foreign import rawParser :: Middleware

type Handler eff a b c = a -> Input b -> Aff eff c

type Input a = { url :: String
               , body :: a
               , params :: StrMap String
               , path :: String 
               , query :: StrMap String
               , headers :: StrMap String
               }

hostEndpoint :: forall a b c eff. (Serializable a, Generic b, Generic c) =>
                  App -> Endpoint a b c -> Handler (express :: EXPRESS, console :: CONSOLE | eff) a b c
                  -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostEndpoint app (Endpoint {method: GET, url}) h = get app url noParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint GET on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ toJSONGeneric defaultOptions a) 
                             (do let i = convert req
                                 qp <- parseQueryParams i
                                 body <- parseBody i
                                 h qp body)
hostEndpoint app (Endpoint {method: POST, url}) h = post app url rawParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint POST on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ toJSONGeneric defaultOptions a) 
                             (do let i = convert req
                                 qp <- parseQueryParams i
                                 body <- parseBody i
                                 h qp body)
hostEndpoint app (Endpoint {method: PUT, url}) h = put app url rawParser handler
  where 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint PUT on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ toJSONGeneric defaultOptions a) 
                             (do let i = convert req
                                 qp <- parseQueryParams i
                                 body <- parseBody i
                                 h qp body)

hostFileUploadEndpoint :: forall eff a b. (Serializable a, Generic b) =>
                      App -> FileUploadEndpoint a b 
                      -> Handler (express :: EXPRESS, console :: CONSOLE | eff) a Buffer b
                      -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFileUploadEndpoint app (FileUploadEndpoint {url}) h = post app url bufferParser handler
  where handler req res = runAff (\err -> do
                                     log $ "Failed hostFileUploadEndpoint on " <> url <> message err
                                     sendStr res $ message err)
                                 (\a -> sendStr res $ toJSONGeneric defaultOptions a) 
                                 (let i = convertBuffer req
                                   in parseQueryParams i >>= \qp -> h qp i)


parseBody :: forall a m. (Generic a, MonadError Error m) => Input String -> m (Input a)
parseBody a = either (\err -> throwError $ error $ show err)
                            (\p -> return $ a { body = p})
                            (readJSONGeneric defaultOptions a.body)

foreign import getParamsImpl :: forall a. (a -> Maybe a) -> Maybe a -> String -> Maybe String
getParams :: String -> Maybe String
getParams = getParamsImpl Just Nothing

parseQueryParams :: forall a b m. (Serializable b, MonadError Error m) => Input a -> m b
parseQueryParams {url} = either throwError
                                return
                                (maybe (Left $ error $ "No params found") 
                                       (\s -> deserialize s)
                                       (getParams url))

hostFile :: forall eff. 
              App -> String -> (Input Unit -> Aff (express :: EXPRESS, console :: CONSOLE | eff) Buffer) 
              -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFile app url f = get app url noParser handler
  where 
    handler req res = runAff (\err -> do
                               log $ "Failed hostFile " <> message err
                               sendStr res $ message err)
                             (\a -> sendBuffer res a)
                             (parseBody (convert req) >>= f)

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
