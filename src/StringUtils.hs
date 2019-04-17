module StringUtils where

import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

textToByteString :: T.Text -> B.ByteString
textToByteString x = TE.encodeUtf8 x

lazyByteStringToLazyText :: L.ByteString -> LT.Text
lazyByteStringToLazyText x = TLE.decodeUtf8 x

stringToLazyText :: String -> LT.Text
stringToLazyText x = LT.pack x