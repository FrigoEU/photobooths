module Main where

import Prelude
import Database.AnyDB (ConnectionInfo (..), Query(..), withConnection, execute_, query_, DB())
import Klikhut.SQL
import Klikhut.Model.Photobooth as PB
 
{-- import Node.Express.Types (Port()) --}
{-- import Node.Express.App (listenHttp, App(), get) --}
{-- import Node.Express.Handler (sendFile, Handler(), getUrl, send, capture, setStatus) --}


import Klikhut.Endpoint
import Klikhut.Server.Core
import Control.Monad.Eff.Exception (Error(), message, error)
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (runAff, Aff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)

import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Encode (encodeJson, EncodeJson)


connectionInfo :: ConnectionInfo
connectionInfo = Sqlite3  
  { filename: "klikhutdb"
  , memory: false }

mybooth :: PB.Photobooth
mybooth = PB.Photobooth { computername: "mycomputername"
                        , alias: "myalias"
                        , defaultprofile: "myprofile" }
yourbooth :: PB.Photobooth
yourbooth = PB.Photobooth { computername: "yourcomputername"
                          , alias: "youralias"
                          , defaultprofile: "yourprofile" }

port :: Int
port = 8080

main = do
  runAff (log <<< show) (const $ log "Initial SQL OK") $ do
    withConnection connectionInfo \conn -> do
      execute_ (dropTable PB.table) conn
      execute_ (createTable PB.table PB.cols) conn
      execute_ (insertPB PB.table mybooth) conn
      execute_ (insertPB PB.table yourbooth) conn
  app <- makeApp
  hostEndpoint app getPhotobooths allPhotobooths
  hostEndpoint app postPhotobooths newPB
  hostEndpoint app putPhotobooths updatePB
  hostFile app "*" static
  listen app port 
  log $ "Starting server on " ++ show port 

static :: forall eff. Input Unit -> Aff (console :: CONSOLE | eff) String
static {url: "/"} = return "./static/index.html"
static {url: "/static/client.js"} = return "./static/client.js"
static {url: a} = return a
{-- static {url: a} = do --}
{--   liftEff $ log $ "Unknown file : " <> a --} 
{--   throwError $ error "Unknown file" --}

insertPB :: forall a. Table -> PB.Photobooth -> Query a
insertPB t (PB.Photobooth m) = insert t [m.computername, m.alias, m.defaultprofile]

allPhotobooths :: forall eff. Input Unit -> Aff (db :: DB | eff) (Array PB.Photobooth)
allPhotobooths _ = withConnection connectionInfo 
                   \conn -> query_ (selectStar PB.table "" :: Query PB.Photobooth) conn

newPB ({body: pb}) = 
  withConnection connectionInfo \conn -> execute_ (insertPB PB.table pb) conn
updatePB ({body: (PB.Photobooth pb)}) = 
  let query = (Query $ "Update " <> show PB.table <> 
              " set alias = '" <> pb.alias <>"', defaultprofile = '" <> pb.defaultprofile <> "' " <>
              " where computername='" <> pb.computername <> "'")
   in withConnection connectionInfo \conn -> execute_ query conn

