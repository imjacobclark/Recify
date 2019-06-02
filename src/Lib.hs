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

import CSVWriter
import qualified RecentlyPlayedMarshall as RPM

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

getCurrentUsersRecentlyPlayedTracks :: B.ByteString -> String -> IO L.ByteString
getCurrentUsersRecentlyPlayedTracks accessToken uri = do
  let options = W.defaults & W.header "Authorization" .~ [(B.pack "Bearer ") <> accessToken] 
  text <- liftIO $ (W.getWith options uri)
  return $ text ^. W.responseBody

getAccessTokenFromPayload :: L.ByteString -> AccessToken
getAccessTokenFromPayload json = AccessToken (json ^. key "access_token" . _String)

getArtistsFromTrack :: RPM.Track -> String
getArtistsFromTrack track = concat . intersperse ", " $ fmap (\artist -> (RPM.artistName artist)) (RPM.artists track)

buildRecentlyPlayedHTMLList :: [RPM.Track] -> String
buildRecentlyPlayedHTMLList [] = []
buildRecentlyPlayedHTMLList (tracks) = concat . intersperse "<br>" $ fmap (\track -> "<li>" ++ (RPM.name track) ++ " - " ++ (getArtistsFromTrack track) ++ "</li>") tracks

getRecentlyPlayedHTMLResponse :: RPM.RecentlyPlayed -> IO LT.Text
getRecentlyPlayedHTMLResponse recentlyPlayed = do
  _ <- writeCsvToDisk recentlyPlayed
  return (stringToLazyText . buildRecentlyPlayedHTMLList . RPM.tracks . RPM.recentlyPlayed $ recentlyPlayed)

getNextRecentlyPlayedTracksHref :: RPM.RecentlyPlayed -> LT.Text
getNextRecentlyPlayedTracksHref recentlyPlayed = stringToLazyText . RPM.next $ recentlyPlayed

buildResponse :: LT.Text -> LT.Text -> ActionM ()
buildResponse recentlyPlayedHTML nextHTML = html $ mconcat [
  "<!DOCTYPE html><html><head><title>Recify</title><meta name='viewport' content='width=device-width, initial-scale=1'><style>body { max-width: 1024px; margin: 0 auto; padding: 10px }; main { width:  100%; };</style></head><body>",
  "<main>",
  "<h1>Recify</h1><hr />", 
  "<p>Here is your recently played track list:</p>",
  "<ul>", recentlyPlayedHTML,"</ul>",
  "<p>Next: ", nextHTML, "</p>",
  "<hr /><footer>Recify and Jacob Clark 2019 &middot; <a href='https://www.github.com/imjacobclark/recify' style='color: #1db954'>Recify on GitHub</a></footer>",
  "</p></main></body></html>"]

recify :: IO ()
recify = scotty port $ do
    get "/" $ do
      html $ mconcat [
        "<!DOCTYPE html><html><head><title>Recify</title><meta name='viewport' content='width=device-width, initial-scale=1'><style>body { max-width: 1024px; margin: 0 auto; padding: 10px }; main { width:  100%; };</style></head><body>",
        "<main>",
        "<h1>Recify</h1><hr />", 
        "<p>In order to proceed you need to give Recify permission to access bits of your Spotify profile.</p>", 
        "<p>---> <a href='/grant' style='color: #1db954'>Grant permission now!</a> <---</p>",
        "<p>Recify will request delegated access to your recently played tracks stored on Spotify. It will use these tracks to analyze and generate recomendations on songs you may like to listen to next.</p>",
        "<hr /><footer>Recify and Jacob Clark 2019 &middot; <a href='https://www.github.com/imjacobclark/recify' style='color: #1db954'>Recify on GitHub</a></footer>",
        "</p></main></body></html>"]  

    get "/grant" $ do
      clientId <- liftIO $ getEnv "clientID"
      status status302
      setHeader "Location" (LT.pack ("https://accounts.spotify.com/authorize?client_id=" ++ clientId ++ "&response_type=" ++ authorizationResponseType ++ "&redirect_uri=" ++ callbackUri ++ "&scope=" ++ authorizationScope))
  
    get "/callback" $ do
      authorizationCode <- fmap AuthorizationCode $ param "code"
      accessTokenPayload <- liftIO $ requestAccessTokenFromAuthorizationCode authorizationCode
      _ <- liftIO . writeFile "./accessToken.txt" . T.unpack . getAccessToken . getAccessTokenFromPayload $ accessTokenPayload
      status status302
      setHeader "X-Forwarded-From" "/callback"
      setHeader "Location" (LT.pack $ "/dashboard")

    get "/dashboard" $ do
      accessTokenFileData <- (liftIO $ DTIO.readFile "./accessToken.txt")
      recentlyPlayedTrackData <- liftIO $ (getCurrentUsersRecentlyPlayedTracks (textToByteString . getAccessToken . AccessToken $ accessTokenFileData) recentlyPlayerUri)
      
      let maybeMarshalledRecentlyPlayed = (RPM.marshallRecentlyPlayedData recentlyPlayedTrackData)

      case maybeMarshalledRecentlyPlayed of
        Right (marshalledRecentlyPlayed) -> do 
          recentlyPlayedHTMLResponse <- liftIO $ getRecentlyPlayedHTMLResponse marshalledRecentlyPlayed
          buildResponse recentlyPlayedHTMLResponse (getNextRecentlyPlayedTracksHref marshalledRecentlyPlayed)
        Left (error) -> html $ mconcat ["Something when wrong getting data from Spotify, refresh to try again."]