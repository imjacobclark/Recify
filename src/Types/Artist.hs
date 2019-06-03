{-# LANGUAGE DeriveGeneric #-}

module Types.Artist where

import qualified Data.ByteString.Lazy as L
import GHC.Generics
import Data.Aeson

data Artist = Artist {
    genres  :: [String]
} deriving (Show, Generic)

instance FromJSON Artist

marshallArtistData :: L.ByteString -> Either String Artist
marshallArtistData artist = eitherDecode artist