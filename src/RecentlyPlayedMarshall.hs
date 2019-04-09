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
  parseJSON (Object recentlyPlayed) = do
    items <- recentlyPlayed .: "items"
    RecentlyPlayed <$> parseJSON items
  parseJSON _                       = fail "No object found..."

instance FromJSON Tracks where
  parseJSON = withArray "items" $ \items ->
    Tracks <$> mapM parseJSON (V.toList items)

instance FromJSON Track where
  parseJSON (Object v) = Track 
    <$> v .: "played_at" 
    <*> (v .: "track" >>= (.: "album") >>= (.: "external_urls") >>= (.: "spotify"))
    <*> (v .: "track" >>= (.: "name"))
  parseJSON _          = fail "no object found"

marshallRecentlyPlayedData :: L.ByteString -> Either String RecentlyPlayed
marshallRecentlyPlayedData recentlyPlayedTracks = eitherDecode recentlyPlayedTracks