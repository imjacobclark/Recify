{-# LANGUAGE OverloadedStrings #-}

module Clients.HttpClient where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Network.Wreq as W
import Control.Monad.IO.Class
import Control.Lens

getWithBearerToken :: B.ByteString -> String -> IO L.ByteString
getWithBearerToken token endpoint = do
    let options = W.defaults & W.header "Authorization" .~ [(B.pack "Bearer ") <> token] 
    text <- liftIO $ (W.getWith options endpoint)
    return $ (text ^. W.responseBody)