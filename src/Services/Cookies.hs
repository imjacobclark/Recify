{-# LANGUAGE OverloadedStrings #-}

module Services.Cookies where

import Control.Monad (forM_)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text as TX
import Data.Text.Lazy as TXL
import qualified Data.Text.Lazy as LT

import Web.Scotty

setCookie :: String -> ActionM ()
setCookie accessToken = setHeader "Set-Cookie" (TXL.intercalate "" . fmap TXL.singleton $ "authToken=" ++ accessToken ++ "; HttpOnly;")

getCookies :: ActionM (Maybe TXL.Text)
getCookies = header "Cookie"

getHead [] = ""
getHead (x:xs) = x

getTail [] = ""
getTail (_:xs) = (getHead xs)

getAccessTokenFromCookies :: ActionM [(LT.Text, LT.Text)]
getAccessTokenFromCookies = do
    cookies <- getCookies
    case cookies of
        Just cookies -> return $ Prelude.filter (
            \tuple -> (fst $ tuple) == LT.pack " authToken") . fmap (
                \c -> (
                    getHead $ (LT.splitOn "=" c), getTail $ (LT.splitOn "=" c))) . LT.splitOn ";" $ cookies
        Nothing -> return $ [("", "")]