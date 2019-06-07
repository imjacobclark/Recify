{-# LANGUAGE OverloadedStrings #-}

module Types.RecentlyPlayedWithArtist where

import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import Data.Aeson
import Data.Either

data Artist = Artist
  { id         :: String
  , href       :: String
  , artistName :: String
  , genres     :: [String]
  } deriving (Show)

data Track = Track
  { playedAt     :: String
  , externalUrls :: String
  , name         :: String
  , artists      :: [Artist]
  , explicit     :: Bool
  } deriving (Show)

data Tracks = Tracks
  { tracks :: [Track]
  } deriving (Show)

data RecentlyPlayedWithArtist = RecentlyPlayedWithArtist
  { recentlyPlayed :: Tracks
  , next           :: String
  } deriving (Show)
