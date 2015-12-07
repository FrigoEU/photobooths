module App.Endpoint (module Endpoint.Client, module App.Endpoint) where

import Prelude

import Network.HTTP.Method (Method(..))

import Data.Tuple (Tuple(..))
import Data.Date(Date())
import Data.Maybe (Maybe())

import App.Model.Photobooth as PB
import App.Model.Event as E
import App.Model.SavedFile as SI
import App.Model.Statistic as S
import App.Model.Date

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

getPhotobooth :: Endpoint String Unit (Maybe PB.Photobooth)
getPhotobooth = Endpoint { method: GET, serverUrl: "/api/photobooths/:cname"
                         , mkClientUrl: \s -> "/api/photobooths/" <> s}

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

attachFile :: FileUploadEndpoint (Tuple Int String) SI.SavedFile
attachFile = FileUploadEndpoint { serverUrl: "/api/attachfiletoevent/:eventid/:name"
                           , mkClientUrl: \(Tuple i name) -> "/api/attachfiletoevent/" <> show i <> "/" <> name}

getNewEvents :: Endpoint (Tuple String Date) Unit (Array E.PartialEvent)
getNewEvents = Endpoint { method: GET, serverUrl: "/api/newevents/:cname/:date"
                        , mkClientUrl: \(Tuple s d) -> "/api/newevents/" <> s <> "/" <> iso8601 d}

getNewFiles :: Endpoint (Tuple String Date) Unit (Array SI.SavedFile)
getNewFiles = Endpoint { method: GET, serverUrl: "/api/newfiles/:cname/:date"
                        , mkClientUrl: \(Tuple s d) -> "/api/newfiles/" <> s <> "/" <> iso8601 d}

------- Statistics ----------------------

getStatistics :: Endpoint String Unit S.AllStatistics
getStatistics = Endpoint { method: GET, serverUrl: "/api/statistics/:cname"
                         , mkClientUrl: \s -> "/api/statistics/" <> s}

postStatistics :: Endpoint Unit S.AllStatistics Unit
postStatistics = Endpoint { method: POST, serverUrl: "/api/statistics"
                          , mkClientUrl: const "/api/statistics"}
                          
------- Profiles ----------------------

getProfiles :: Endpoint Unit Unit (Array (Tuple String (Array String)))
getProfiles = Endpoint { method: GET, serverUrl: "/api/profiles"
                       , mkClientUrl: const "/api/profiles"}

getProfileFiles :: Endpoint (Tuple String String) Unit (Array String)
getProfileFiles = Endpoint {method: GET, serverUrl: "/api/profiles/:cname/:pname"
                           , mkClientUrl: \(Tuple cname pname) -> 
                                             "/api/profiles/" <> cname <> "/" <> pname}
