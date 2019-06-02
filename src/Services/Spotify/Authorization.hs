{-# LANGUAGE OverloadedStrings #-}

module Services.Spotify.Authorization where

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

getAccessTokenFromPayload :: L.ByteString -> AccessToken
getAccessTokenFromPayload json = AccessToken (json ^. key "access_token" . _String)

writeAccessTokenToDisk :: L.ByteString -> IO ()
writeAccessTokenToDisk accessToken = writeFile "./accessToken.txt" . T.unpack . getAccessToken . getAccessTokenFromPayload $ accessToken