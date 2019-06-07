module Types.SpotifyAuthorization where

import qualified Data.Text as T

newtype AuthorizationCode = AuthorizationCode { getAuthorizationCode :: String }

newtype AccessToken = AccessToken { getAccessToken :: T.Text }
