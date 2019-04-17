{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( recify,
      getAccessTokenFromPayload
    ) where

import Web.Scotty
import Network.HTTP.Types (status302)
import Control.Monad.IO.Class
import qualified Network.Wreq as W
import Control.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.IO as DTIO

import Data.Aeson
import Data.Either
import Data.Aeson.Lens (_String, key)
import Data.List

import System.Environment

import StringUtils
import RecentlyPlayedMarshall

newtype AuthorizationCode = AuthorizationCode {
  getAuthorizationCode :: String
}

newtype AccessToken = AccessToken {
  getAccessToken :: T.Text
}

authorizationScope = "user-read-recently-played, user-top-read"
authorizationResponseType = "code"

callbackUri = "http://localhost:3000/callback"
grantType = "authorization_code"
accessTokenRequestUri = "https://accounts.spotify.com/api/token"
recentlyPlayerUri = "https://api.spotify.com/v1/me/player/recently-played"

port = 3000

requestAccessTokenFromAuthorizationCode :: AuthorizationCode -> IO L.ByteString
requestAccessTokenFromAuthorizationCode authorizationCode = do 
  bearer <- liftIO $ getEnv "bearer"
  let options = W.defaults & W.header "Authorization" .~ [(B.pack bearer)]
  let payload = [("code" :: B.ByteString, B.pack $ getAuthorizationCode authorizationCode :: B.ByteString)
                  , ("grant_type" :: B.ByteString, B.pack $ grantType :: B.ByteString)
                  , ("redirect_uri" :: B.ByteString, B.pack $ callbackUri :: B.ByteString)
                ]
  text <- liftIO $ (W.postWith options accessTokenRequestUri payload)
  return $ text ^. W.responseBody

getCurrentUsersRecentlyPlayedTracks :: B.ByteString -> IO L.ByteString
getCurrentUsersRecentlyPlayedTracks accessToken = do
  let options = W.defaults & W.header "Authorization" .~ [(B.pack "Bearer ") <> accessToken] 
  text <- liftIO $ (W.getWith options recentlyPlayerUri)
  return $ text ^. W.responseBody

getAccessTokenFromPayload :: L.ByteString -> AccessToken
getAccessTokenFromPayload json = AccessToken (json ^. key "access_token" . _String)

buildRecentlyPlayedHTMLList :: [Track] -> String
buildRecentlyPlayedHTMLList [] = []
buildRecentlyPlayedHTMLList (xs) = concat . intersperse "<br>" $ fmap (\x -> "<li>" ++ (name x) ++ "</li>") xs

recify :: IO ()
recify = scotty port $ do
    get "/" $ do
      html $ mconcat ["<h1>Welcome to Recify</h1>", "<p>In order to proceed, you need to <a href='/login'>Login</a> to Spotify</a></p>"]  

    get "/login" $ do
      clientId <- liftIO $ getEnv "clientID"
      status status302
      setHeader "Location" (LT.pack ("https://accounts.spotify.com/authorize?client_id=" ++ clientId ++ "&response_type=" ++ authorizationResponseType ++ "&redirect_uri=" ++ callbackUri ++ "&scope=" ++ authorizationScope))
    
    get "/dashboard" $ do
      accessToken <- (liftIO $ DTIO.readFile "./accessToken.txt")
      recentlyPlayedTrackData <- liftIO . getCurrentUsersRecentlyPlayedTracks . textToByteString . getAccessToken . AccessToken $ accessToken
      
      case (marshallRecentlyPlayedData recentlyPlayedTrackData) of
        Right (marshalledRecentlyPlayed) -> html $ mconcat ["<ul>", (stringToLazyText . buildRecentlyPlayedHTMLList . tracks . recentlyPlayed $ marshalledRecentlyPlayed), "</ul>"]
        Left (error) -> html $ mconcat ["Fail"]

    get "/callback" $ do
      authorizationCode <- fmap AuthorizationCode $ param "code"
      accessTokenPayload <- liftIO $ requestAccessTokenFromAuthorizationCode authorizationCode
      _ <- liftIO . writeFile "./accessToken.txt" . T.unpack . getAccessToken . getAccessTokenFromPayload $ accessTokenPayload
      status status302
      setHeader "Location" (LT.pack $ "/dashboard")