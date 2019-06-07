module Utils.String where

import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

textToByteString :: T.Text -> B.ByteString
textToByteString = TE.encodeUtf8

lazyByteStringToLazyText :: L.ByteString -> LT.Text
lazyByteStringToLazyText = TLE.decodeUtf8

stringToLazyText :: String -> LT.Text
stringToLazyText = LT.pack
