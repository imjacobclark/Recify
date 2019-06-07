{-# LANGUAGE OverloadedStrings #-}

module Services.Spotify.Authorization where

import Web.Scotty
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Aeson.Lens (_String, key)
import Data.Aeson
import Control.Lens
import qualified Data.Text.IO as DTIO
import Control.Monad.IO.Class
import Data.ByteString.Internal

import Utils.String
import Types.SpotifyAuthorization

import Services.Cookies

getAccessTokenFromPayload :: L.ByteString -> AccessToken
getAccessTokenFromPayload json = AccessToken (json ^. key "access_token" . _String)


writeAccessTokenCookies :: L.ByteString -> Web.Scotty.ActionM ()
writeAccessTokenCookies = setCookie . T.unpack . getAccessToken . getAccessTokenFromPayload