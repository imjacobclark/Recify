{-# LANGUAGE OverloadedStrings #-}

module Services.Cookies where

import Control.Monad (forM_)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text as TX
import Data.Text.Lazy as TXL

import Web.Scotty

setCookie :: String -> ActionM ()
setCookie accessToken = setHeader "Set-Cookie" (TXL.intercalate "" . fmap TXL.singleton $ "authToken=" ++ accessToken ++ "; HttpOnly;")

getCookies :: ActionM (Maybe TXL.Text)
getCookies = header "Cookie"