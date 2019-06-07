{-# LANGUAGE OverloadedStrings #-}

module Recify where

import Web.Scotty
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

  scotty port $ do
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

    get "/dashboard" $ do
      cookies <- getCookies
      accessTokenData <- case cookies of
        Just cookies -> return $ DT.pack . LT.unpack $ ((LT.splitOn "=" ((LT.splitOn ";" $ cookies) !! 0)) !! 1) -- this is pretty bad, we check for the actual token, there is no guarantees this will always work...
        Nothing -> return $ ""

      recentlyPlayedTrackData <- liftIO . fetchRecentlyPlayedTracks $ accessTokenData
      let maybeMarshalledRecentlyPlayed = marshallRecentlyPlayedData recentlyPlayedTrackData

      case maybeMarshalledRecentlyPlayed of
        Right marshalledRecentlyPlayed -> do
          maybeMarshalledArtistsData <- liftIO . getArtistData accessTokenData $ marshalledRecentlyPlayed

          artists <- case maybeMarshalledArtistsData of
            Right marshalledArtistsData -> return marshalledArtistsData
            Left error                  -> return [] -- TODO this needs to error, no point continuing

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

          recentlyPlayedHTMLResponse <- liftIO . getRecentlyPlayedHTMLResponse $ recentlyPlayedWithArtist

          let nextRecentlyPlayedTracksHref = getNextRecentlyPlayedTracksHref recentlyPlayedWithArtist

          response <- buildResponse recentlyPlayedHTMLResponse nextRecentlyPlayedTracksHref
          html $ mconcat [response]
        Left error ->
          html $ mconcat ["Something went wrong getting data from Spotify, refresh to try again."]
