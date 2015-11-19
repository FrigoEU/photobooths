module App.Endpoint (module Endpoint.Client, module App.Endpoint) where

import Prelude

import Network.HTTP.Method (Method(..))

import Data.Tuple (Tuple(..))

import App.Model.Photobooth as PB
import App.Model.Event as E
import App.Model.SavedImage as SI

import Endpoint.Client

------- Photobooths --------------------

getPhotobooths :: Endpoint Unit Unit (Array PB.Photobooth)
getPhotobooths = Endpoint { method: GET, serverUrl: "/api/photobooths"
                          , mkClientUrl: const "/api/photobooths"} 

postPhotobooths :: Endpoint Unit PB.Photobooth PB.Photobooth
postPhotobooths = Endpoint { method: POST, serverUrl: "/api/photobooths"
                           , mkClientUrl: const "/api/photobooths"}

putPhotobooths :: Endpoint Unit PB.Photobooth PB.Photobooth
putPhotobooths = Endpoint { method: PUT, serverUrl: "/api/photobooths"
                          , mkClientUrl: const "/api/photobooths"}

------- Events -------------------------

getEvents :: Endpoint String Unit (Array E.Event)
getEvents = Endpoint { method: GET, serverUrl: "/api/events/:cname"
                     , mkClientUrl: \s -> "/api/events/" <> s} 

postEvents :: Endpoint Unit E.Event E.Event
postEvents = Endpoint { method: POST, serverUrl: "/api/events"
                      , mkClientUrl: const "/api/events"}

putEvents :: Endpoint Unit E.Event E.Event
putEvents = Endpoint { method: PUT, serverUrl: "/api/events"
                     , mkClientUrl: const "/api/events"}

attachImage :: FileEndpoint (Tuple Int String) SI.SavedImage
attachImage = FileEndpoint { serverUrl: "/api/attachfiletoevent/:eventid/:name"
                           , mkClientUrl: \(Tuple i name) -> "/api/attachfiletoevent/" <> show i <> "/" <> name}

------- Statistics ----------------------

