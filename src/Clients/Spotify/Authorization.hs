{-# LANGUAGE OverloadedStrings #-}

module Clients.Spotify.Authorization where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Network.Wreq as W
import System.Environment
import Control.Monad.IO.Class
import Control.Lens

import Services.Spotify.Authorization
import Types.SpotifyAuthorization

grantType = "authorization_code"
accessTokenRequestUri = "https://accounts.spotify.com/api/token"

exchangeAccessTokenForAuthorizationCode :: AuthorizationCode -> String -> IO L.ByteString
exchangeAccessTokenForAuthorizationCode authorizationCode callbackUri = do 
  bearer <- liftIO $ getEnv "bearer"
  let options = W.defaults & W.header "Authorization" .~ [(B.pack bearer)]
  let payload = [("code" :: B.ByteString, B.pack $ getAuthorizationCode authorizationCode :: B.ByteString)
                  , ("grant_type" :: B.ByteString, B.pack $ grantType :: B.ByteString)
                  , ("redirect_uri" :: B.ByteString, B.pack $ callbackUri :: B.ByteString)
                ]
  text <- liftIO $ (W.postWith options accessTokenRequestUri payload)
  return $ text ^. W.responseBody