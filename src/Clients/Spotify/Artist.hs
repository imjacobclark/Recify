{-# LANGUAGE OverloadedStrings #-}

module Clients.Spotify.Artist where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Network.Wreq as W
import Control.Monad.IO.Class
import Control.Lens
import Clients.HttpClient

fetchArtistById :: String -> B.ByteString -> IO L.ByteString
fetchArtistById id token = getWithBearerToken token $ "https://api.spotify.com/v1/artists/" ++ id

  