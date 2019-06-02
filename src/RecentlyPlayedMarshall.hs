{-# LANGUAGE OverloadedStrings #-}

module RecentlyPlayedMarshall where

import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import Data.Aeson
import Data.Either

data Artist = Artist {
  id :: String
  , href :: String
  , artistName :: String
} deriving (Show)

data Track = Track {
  playedAt :: String
  , externalUrls :: String
  , name :: String
  , artists :: [Artist]
  , explicit :: Bool
} deriving (Show)

data Tracks = Tracks {
  tracks :: [Track]
} deriving (Show)

data RecentlyPlayed = RecentlyPlayed {
  recentlyPlayed :: Tracks
  , next :: String
} deriving (Show)

instance FromJSON RecentlyPlayed where
  parseJSON = withObject "items" $ \recentlyPlayed -> RecentlyPlayed 
    <$> recentlyPlayed .: "items"
    <*> recentlyPlayed .: "next"

instance FromJSON Tracks where
  parseJSON = withArray "items" $ \items -> Tracks 
    <$> mapM parseJSON (V.toList items)

instance FromJSON Track where
  parseJSON = withObject "tracks" $ \tracks -> Track 
    <$> tracks .: "played_at" 
    <*> (tracks .: "track" >>= (.: "album") >>= (.: "external_urls") >>= (.: "spotify"))
    <*> (tracks .: "track" >>= (.: "name"))
    <*> (tracks .: "track" >>= (.: "artists"))
    <*> (tracks .: "track" >>= (.: "explicit"))

instance FromJSON Artist where
  parseJSON = withObject "artists" $ \artists -> Artist
    <$> artists .: "id"
    <*> artists .: "href"
    <*> artists .: "name"

marshallRecentlyPlayedData :: L.ByteString -> Either String RecentlyPlayed
marshallRecentlyPlayedData recentlyPlayedTracks = eitherDecode recentlyPlayedTracks