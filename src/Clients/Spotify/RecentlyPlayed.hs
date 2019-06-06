{-# LANGUAGE OverloadedStrings #-}

module Clients.Spotify.RecentlyPlayed where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Network.Wreq as W
import Control.Monad.IO.Class
import Control.Lens
import Clients.HttpClient

fetchRecentlyPlayedTracks :: B.ByteString -> IO L.ByteString
fetchRecentlyPlayedTracks token = getWithBearerToken token "https://api.spotify.com/v1/me/player/recently-played"