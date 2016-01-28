module App.Endpoint (module Endpoint.Client, module App.Endpoint) where

import Prelude (Unit)

import Network.HTTP.Method (Method(..))

import Data.Tuple (Tuple(..))
import Data.Date(Date())
import Data.Maybe (Maybe())

import App.Model.Photobooth as PB
import App.Model.Event as E
import App.Model.SavedFile as SI
import App.Model.Statistic as S
import App.Model.Date (fromLocalDatetime, fromLocalDatetimeImpl, iso8601, toLocalDatetime)

import Endpoint.Client (Endpoint(Endpoint), FileUploadEndpoint(FileUploadEndpoint), execEndpoint, execEndpoint_, execFileUploadEndpoint, fileToBlob, parseOrThrow)

------- Photobooths --------------------

getPhotobooth :: Endpoint String Unit (Maybe PB.Photobooth)
getPhotobooth = Endpoint { method: GET, url: "/api/photobooths/cname"}

getPhotobooths :: Endpoint Unit Unit (Array PB.Photobooth)
getPhotobooths = Endpoint { method: GET, url: "/api/photobooths/all"} 

postPhotobooths :: Endpoint Unit PB.Photobooth PB.Photobooth
postPhotobooths = Endpoint { method: POST, url: "/api/photobooths"}

putPhotobooths :: Endpoint Unit PB.Photobooth PB.Photobooth
putPhotobooths = Endpoint { method: PUT, url: "/api/photobooths" }

------- Events -------------------------

getEvents :: Endpoint String Unit (Array E.Event)
getEvents = Endpoint { method: GET, url: "/api/events/cname"} 

postEvents :: Endpoint Unit E.Event E.Event
postEvents = Endpoint { method: POST, url: "/api/events"}

putEvents :: Endpoint Unit E.Event E.Event
putEvents = Endpoint { method: PUT, url: "/api/events"}

attachFile :: FileUploadEndpoint (Tuple Int String) SI.SavedFile
attachFile = FileUploadEndpoint { url: "/api/attachfiletoevent"}

getNewEvents :: Endpoint (Tuple String Date) Unit (Array E.PartialEvent)
getNewEvents = Endpoint { method: GET, url: "/api/newevents"}

getNewFiles :: Endpoint (Tuple String Date) Unit (Array SI.SavedFile)
getNewFiles = Endpoint { method: GET, url: "/api/newfiles"}

------- Statistics ----------------------

getStatistics :: Endpoint String Unit S.AllStatistics
getStatistics = Endpoint { method: GET, url: "/api/statistics/cname"}

postStatistics :: Endpoint Unit S.AllStatistics Unit
postStatistics = Endpoint { method: POST, url: "/api/statistics"}
                          
------- Profiles ----------------------

getProfiles :: Endpoint Unit Unit (Array (Tuple String (Array String)))
getProfiles = Endpoint { method: GET, url: "/api/profiles/all"}

getProfileFiles :: Endpoint (Tuple String String) Unit (Array String)
getProfileFiles = Endpoint {method: GET, url: "/api/profiles/files" }
