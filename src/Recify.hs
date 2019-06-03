{-# LANGUAGE OverloadedStrings #-}

module Recify where

import Web.Scotty
import Network.HTTP.Types (status302)
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as DTIO
import Data.Either
import System.Environment

import Utils.String
import Writers.CSV
import Writers.HTML

import Services.Spotify.Authorization

import Clients.Spotify.Authorization
import Clients.Spotify.RecentlyPlayed
import Clients.Spotify.Artist

import Types.SpotifyAuthorization
import Types.RecentlyPlayed
import Types.Artist

authorizationScope = "user-read-recently-played, user-top-read"
authorizationResponseType = "code"
callbackUri = "http://localhost:3000/callback"

port = 3000

recify :: IO ()
recify = scotty port $ do
    get "/" $ do
      homeHtml <- (liftIO $ DTIO.readFile "./static/home.html")
      html $ mconcat [(LT.fromStrict homeHtml)]  

    get "/grant" $ do
      clientId <- liftIO $ getEnv "clientID"
      status status302
      setHeader "Location" (LT.pack ("https://accounts.spotify.com/authorize?client_id=" ++ clientId ++ "&response_type=" ++ authorizationResponseType ++ "&redirect_uri=" ++ Recify.callbackUri ++ "&scope=" ++ authorizationScope))
  
    get "/callback" $ do
      authorizationCode <- fmap AuthorizationCode $ param "code"
      accessTokenPayload <- liftIO $ requestAccessTokenFromAuthorizationCode authorizationCode
      _ <- liftIO . writeAccessTokenToDisk $ accessTokenPayload
      status status302
      setHeader "X-Forwarded-From" "/callback"
      setHeader "Location" (LT.pack $ "/dashboard")

    get "/dashboard" $ do
      accessTokenFileData <- (liftIO $ DTIO.readFile "./accessToken.txt")

      recentlyPlayedTrackData <- liftIO $ (getCurrentUsersRecentlyPlayedTracks (textToByteString . getAccessToken . AccessToken $ accessTokenFileData))
      let maybeMarshalledRecentlyPlayed = (marshallRecentlyPlayedData recentlyPlayedTrackData)
    
      case maybeMarshalledRecentlyPlayed of
        Right (marshalledRecentlyPlayed) -> do 
          recentlyPlayedHTMLResponse <- liftIO $ getRecentlyPlayedHTMLResponse marshalledRecentlyPlayed

          let nextRecentlyPlayedTracksHref = getNextRecentlyPlayedTracksHref marshalledRecentlyPlayed
          
          -- Get Artist Data Start
          artistData <- liftIO $ ((getArtist (Types.RecentlyPlayed.id (artists ((tracks . recentlyPlayed $ marshalledRecentlyPlayed) !! 0) !! 0))) (textToByteString . getAccessToken . AccessToken $ accessTokenFileData))
          let maybeMarshalledArtist = (marshallArtistData artistData)

          artist <- case maybeMarshalledArtist of
            Right (marshalledArtist) -> do  
              return $ (genres marshalledArtist)
            Left (error) -> do
              return $ ["Something went wrong getting data from Spotify, refresh to try again."]

          liftIO $ putStrLn (artist !! 0)
          liftIO $ putStrLn (artist !! 1)
          liftIO $ putStrLn (artist !! 2)
          -- Get Arist Data Stop

          buildResponse recentlyPlayedHTMLResponse nextRecentlyPlayedTracksHref
        Left (error) -> do
          html $ mconcat ["Something went wrong getting data from Spotify, refresh to try again."]