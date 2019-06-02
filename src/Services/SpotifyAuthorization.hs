{-# LANGUAGE OverloadedStrings #-}

module Services.SpotifyAuthorization where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Aeson.Lens (_String, key)
import Data.Aeson
import Control.Lens
import qualified Data.Text.IO as DTIO
import Control.Monad.IO.Class
import Data.ByteString.Internal

import Utils.String

newtype AuthorizationCode = AuthorizationCode {
  getAuthorizationCode :: String
}

newtype AccessToken = AccessToken {
  getAccessToken :: T.Text
}

getAccessTokenFromPayload :: L.ByteString -> AccessToken
getAccessTokenFromPayload json = AccessToken (json ^. key "access_token" . _String)

writeAccessTokenToDisk :: L.ByteString -> IO ()
writeAccessTokenToDisk accessToken = writeFile "./accessToken.txt" . T.unpack . getAccessToken . getAccessTokenFromPayload $ accessToken