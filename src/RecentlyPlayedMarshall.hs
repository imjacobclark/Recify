{-# LANGUAGE OverloadedStrings #-}

module RecentlyPlayedMarshall where

import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import Data.Aeson

data Track = Track {
  playedAt :: String
  , externalUrls :: String
  , name :: String
} deriving (Show)

newtype Tracks = Tracks {
  tracks :: [Track] 
} deriving (Show)

newtype RecentlyPlayed = RecentlyPlayed {
  recentlyPlayed :: Tracks 
} deriving (Show)

instance FromJSON RecentlyPlayed where
  parseJSON = withObject "items" $ \recentlyPlayed -> RecentlyPlayed 
    <$> recentlyPlayed .: "items"

instance FromJSON Tracks where
  parseJSON = withArray "items" $ \items -> Tracks 
    <$> mapM parseJSON (V.toList items)

instance FromJSON Track where
  parseJSON = withObject "tracks" $ \tracks -> Track 
    <$> tracks .: "played_at" 
    <*> (tracks .: "track" >>= (.: "album") >>= (.: "external_urls") >>= (.: "spotify"))
    <*> (tracks .: "track" >>= (.: "name"))

marshallRecentlyPlayedData :: L.ByteString -> Either String RecentlyPlayed
marshallRecentlyPlayedData recentlyPlayedTracks = eitherDecode recentlyPlayedTracks