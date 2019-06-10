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

log = liftIO . show $ putStrLn

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

    get "/dashboard" $ do
      Recify.log "GET /dashboard"
      accessTokenData <- getAccessTokenFromCookies
      Recify.log "Got access token data from cookies"
      let cookie = DT.pack . LT.unpack . snd $ accessTokenData !! 0 -- need to check if anything exists in the array and die if not 
      Recify.log "Got access token"
      let accessToken = textToByteString . getAccessToken . AccessToken $ cookie -- if this is empty we need to stop as authorization hasn't occoured 
      Recify.log "Parsed access token"

      recentlyPlayedTrackData <- liftIO . fetchRecentlyPlayedTracks $ accessToken
      Recify.log "Fetched recently played tracks"
      let maybeMarshalledRecentlyPlayed = marshallRecentlyPlayedData recentlyPlayedTrackData
      Recify.log "Marshalled recently played tracks"

      case maybeMarshalledRecentlyPlayed of
        Right marshalledRecentlyPlayed -> do
          maybeMarshalledArtistsData <- liftIO . getArtistData accessToken $ marshalledRecentlyPlayed
          Recify.log "Fetched and marshalled artists"

          artists <- case maybeMarshalledArtistsData of
            Right marshalledArtistsData -> return marshalledArtistsData
            Left error                  -> return [] -- TODO this needs to error, no point continuing
          Recify.log "Unwrapped artist data"

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
          Recify.log "Created recentlyPlayedWithArtist data structure"

          recentlyPlayedHTMLResponse <- liftIO . getRecentlyPlayedHTMLResponse $ recentlyPlayedWithArtist
          Recify.log "Produced HTML response"

          let nextRecentlyPlayedTracksHref = getNextRecentlyPlayedTracksHref recentlyPlayedWithArtist
          Recify.log "Produced next link HTML response"

          response <- buildResponse recentlyPlayedHTMLResponse nextRecentlyPlayedTracksHref
          html $ mconcat [response]
        Left error ->
          html $ mconcat ["Something went wrong getting data from Spotify, refresh to try again."]
