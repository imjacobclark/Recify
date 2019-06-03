{-# LANGUAGE OverloadedStrings #-}

module Clients.Spotify.Artist where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Network.Wreq as W
import System.Environment
import Control.Monad.IO.Class
import Control.Lens

recentlyPlayerUri = "https://api.spotify.com/v1/artists/"

getArtist :: String -> B.ByteString -> IO L.ByteString
getArtist id accessToken = do
  let options = W.defaults & W.header "Authorization" .~ [(B.pack "Bearer ") <> accessToken] 
  text <- liftIO $ (W.getWith options (recentlyPlayerUri ++ id))
  return $ text ^. W.responseBody