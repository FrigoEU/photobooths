module Klikhut.Model.Photobooth where

import Prelude
import Klikhut.SQL
import Data.Foreign.Class
import Data.Generic
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Lens (Lens(), lens)

data Photobooth = Photobooth { computername   :: String
                             , alias          :: String
                             , defaultprofile :: String }

derive instance genericPhotobooth :: Generic Photobooth

instance encodeJsonPhotobooth :: EncodeJson Photobooth where encodeJson = gEncodeJson
instance decodeJsonPhotobooth :: DecodeJson Photobooth where decodeJson = gDecodeJson
instance eqPhotobooth         :: Eq Photobooth where eq = gEq 
instance showPhotobooth       :: Show Photobooth where show = gShow

instance photoboothIsForeign :: IsForeign Photobooth where
  read obj = do
    cn <- readProp "computername" obj
    al <- readProp "alias" obj
    dp <- readProp "defaultprofile" obj
    return $ Photobooth {computername: cn, alias: al, defaultprofile: dp}

table = Table "PHOTOBOOTHS"
cols = [FieldDef "computername" $ Char 50, 
        FieldDef "alias" $ Char 50,
        FieldDef "defaultprofile" $ Char 50]

_computername :: forall a b o. Lens {computername :: a | o} {computername :: b | o} a b
_computername = lens _.computername (_ {computername = _})

_alias :: forall a b o. Lens {alias :: a | o} {alias :: b | o} a b
_alias = lens _.alias (_ {alias = _})

_defaultprofile :: forall a b o. Lens {defaultprofile :: a | o} {defaultprofile :: b | o} a b
_defaultprofile = lens _.defaultprofile (_ {defaultprofile = _})

_Photobooth = lens  (\(Photobooth a) -> a) (\_ a -> Photobooth a)
