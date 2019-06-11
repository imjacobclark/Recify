{-# LANGUAGE OverloadedStrings #-}

module Recify where

import Web.Scotty
import Data.Default.Class (def)
import Network.Wai.Handler.Warp (setPort)
import Network.HTTP.Types (status302)

import Control.Monad.IO.Class
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as DTIO
import qualified Data.Text as DT
import Data.Either
import System.Environment

import Utils.String
import Writers.CSV
import Writers.HTML

import Services.Spotify.Authorization
import Services.Spotify.Artist
import Services.Cookies

import Clients.Spotify.Authorization
import Clients.Spotify.RecentlyPlayed

import Types.SpotifyAuthorization
import Types.RecentlyPlayed
import Types.Artist
import qualified Types.RecentlyPlayedWithArtist as RPWA

authorizationScope = "user-read-recently-played, user-top-read"
authorizationResponseType = "code"

recify :: IO ()
recify = do
  port <- fmap read $ getEnv "PORT"
  fqdn <- liftIO $ getEnv "fqdn"

  let scottyOptions = def { settings = setPort port $ settings def }

  scottyOpts scottyOptions $ do
    get "/" $ do
      homeHtml <- liftIO . DTIO.readFile $ "./static/home.html"
      html $ mconcat [(LT.fromStrict homeHtml)]

    get "/grant" $ do
      clientId <- liftIO . getEnv $ "clientID"
      status status302
      let location
            = "https://accounts.spotify.com/authorize?client_id=" ++ clientId
            ++ "&response_type=" ++ authorizationResponseType
            ++ "&redirect_uri=" ++ fqdn
            ++ "/callback&scope=" ++ authorizationScope
      setHeader "Location" $ LT.pack location

    get "/callback" $ do
      authorizationCode <- fmap AuthorizationCode $ param "code"
      accessToken <- liftIO $ exchangeAccessTokenForAuthorizationCode authorizationCode fqdn
      writeAccessTokenCookies accessToken
      status status302
      setHeader "X-Forwarded-From" "/callback"
      setHeader "Location" $ LT.pack "/dashboard"

    get "/csv" $ do
      file <- liftIO $ readFile "./data/recentlyPlayed.csv"
      html $ mconcat ["<pre>", LT.pack file, "</pre>"]

    get "/dashboard" $ do
      liftIO $ putStrLn "Accepting request for /dashboard..."
      accessTokenData <- getAccessTokenFromCookies
      liftIO $ putStrLn ("Got access token " ++ (show accessTokenData))
      let cookie = DT.pack . LT.unpack . snd $ accessTokenData !! 0 -- need to check if anything exists in the array and die if not 
      liftIO $ putStrLn ("Parsed access token " ++ (show cookie))
      let accessToken = textToByteString . getAccessToken . AccessToken $ cookie -- if this is empty we need to stop as authorization hasn't occoured 
      liftIO $ putStrLn ("Marshalled access token " ++ (show accessToken))
      
      recentlyPlayedTrackData <- liftIO . fetchRecentlyPlayedTracks $ accessToken
      liftIO $ putStrLn ("Fetched recently played tracks" ++ (show recentlyPlayedTrackData))
      let maybeMarshalledRecentlyPlayed = marshallRecentlyPlayedData recentlyPlayedTrackData
      liftIO $ putStrLn ("Marshalled recently played tracks" ++ (show maybeMarshalledRecentlyPlayed))
      
      case maybeMarshalledRecentlyPlayed of
        Right marshalledRecentlyPlayed -> do
          maybeMarshalledArtistsData <- liftIO . getArtistData accessToken $ marshalledRecentlyPlayed
          liftIO $ putStrLn "Fetched artist data"

          artists <- case maybeMarshalledArtistsData of
            Right marshalledArtistsData -> do 
              liftIO $ putStrLn "Right marshalled artist data"
              return marshalledArtistsData
            Left error                  -> do
              liftIO $ putStrLn error
              return [] -- TODO this needs to error, no point continuing
          liftIO $ putStrLn ("Marshalled artist data" ++ (show artists))

          let recentlyPlayedWithArtist = RPWA.RecentlyPlayedWithArtist {
            RPWA.recentlyPlayed = RPWA.Tracks {
              RPWA.tracks = fmap (\track -> RPWA.Track {
                RPWA.name = name track,
                RPWA.externalUrls = externalUrls track,
                RPWA.explicit = explicit track,
                RPWA.artists = fmap (\artist -> RPWA.Artist {
                  RPWA.id = Types.RecentlyPlayed.id artist,
                  RPWA.href = href artist,
                  RPWA.artistName = artistName artist,
                  RPWA.genres = concat . fmap Types.Artist.genres . filter (\artistToCompound -> Types.Artist.id artistToCompound == Types.RecentlyPlayed.id artist) $ artists
                }) $ Types.RecentlyPlayed.artists track,
                RPWA.playedAt = playedAt track
              }) . Types.RecentlyPlayed.tracks . recentlyPlayed $ marshalledRecentlyPlayed
            },
            RPWA.next = Types.RecentlyPlayed.next $ marshalledRecentlyPlayed
          }
          liftIO $ putStrLn ("Marshalled recentlyPlayedWithArtist data " ++ (show recentlyPlayedWithArtist))

          recentlyPlayedHTMLResponse <- liftIO . getRecentlyPlayedHTMLResponse $ recentlyPlayedWithArtist
          liftIO $ putStrLn ("recentlyPlayedHTMLResponse " ++ (show recentlyPlayedHTMLResponse))
          -- let nextRecentlyPlayedTracksHref = getNextRecentlyPlayedTracksHref recentlyPlayedWithArtist
          -- liftIO $ putStrLn ("Created HTML response" ++ (show nextRecentlyPlayedTracksHref))

          -- response <- buildResponse recentlyPlayedHTMLResponse nextRecentlyPlayedTracksHref
          -- liftIO $ putStrLn ("Built HTML response" ++ (show response))

          html $ mconcat ["test"]
        Left error ->
          html $ mconcat ["Something went wrong getting data from Spotify, refresh to try again."]
