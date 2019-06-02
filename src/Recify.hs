{-# LANGUAGE OverloadedStrings #-}

module Recify
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

import Utils.String
import Writers.CSV
import Writers.HTML
import qualified Types.RecentlyPlayed as RP
import Services.SpotifyAuthorization

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

getCurrentUsersRecentlyPlayedTracks :: B.ByteString -> String -> IO L.ByteString
getCurrentUsersRecentlyPlayedTracks accessToken uri = do
  let options = W.defaults & W.header "Authorization" .~ [(B.pack "Bearer ") <> accessToken] 
  text <- liftIO $ (W.getWith options uri)
  return $ text ^. W.responseBody

recify :: IO ()
recify = scotty port $ do
    get "/" $ do
      homeHtml <- (liftIO $ DTIO.readFile "./static/home.html")
      html $ mconcat [(LT.fromStrict homeHtml)]  

    get "/grant" $ do
      clientId <- liftIO $ getEnv "clientID"
      status status302
      setHeader "Location" (LT.pack ("https://accounts.spotify.com/authorize?client_id=" ++ clientId ++ "&response_type=" ++ authorizationResponseType ++ "&redirect_uri=" ++ callbackUri ++ "&scope=" ++ authorizationScope))
  
    get "/callback" $ do
      authorizationCode <- fmap AuthorizationCode $ param "code"
      accessTokenPayload <- liftIO $ requestAccessTokenFromAuthorizationCode authorizationCode
      _ <- liftIO . writeAccessTokenToDisk $ accessTokenPayload
      status status302
      setHeader "X-Forwarded-From" "/callback"
      setHeader "Location" (LT.pack $ "/dashboard")

    get "/dashboard" $ do
      accessTokenFileData <- (liftIO $ DTIO.readFile "./accessToken.txt")
      recentlyPlayedTrackData <- liftIO $ (getCurrentUsersRecentlyPlayedTracks (textToByteString . getAccessToken . AccessToken $ accessTokenFileData) recentlyPlayerUri)
      
      let maybeMarshalledRecentlyPlayed = (RP.marshallRecentlyPlayedData recentlyPlayedTrackData)

      case maybeMarshalledRecentlyPlayed of
        Right (marshalledRecentlyPlayed) -> do 
          recentlyPlayedHTMLResponse <- liftIO $ getRecentlyPlayedHTMLResponse marshalledRecentlyPlayed
          buildResponse recentlyPlayedHTMLResponse (getNextRecentlyPlayedTracksHref marshalledRecentlyPlayed)
        Left (error) -> html $ mconcat ["Something when wrong getting data from Spotify, refresh to try again."]