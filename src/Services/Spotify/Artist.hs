{-# LANGUAGE OverloadedStrings #-}

module Services.Spotify.Artist where

import Data.ByteString.Internal 

import Types.RecentlyPlayed
import Types.Artist

import Clients.Spotify.Artist

getArtistData :: Data.ByteString.Internal.ByteString -> RecentlyPlayed -> IO (Either String [Types.Artist.Artist])
getArtistData accessToken marshalledRecentlyPlayed = do
  let artistIds = concatMap (fmap Types.RecentlyPlayed.id . artists) . tracks . recentlyPlayed $ marshalledRecentlyPlayed
  artistsData <- sequence $ fmap (\id -> fetchArtistById id accessToken) artistIds
  return . sequence $ (fmap (marshallArtistData) artistsData)

