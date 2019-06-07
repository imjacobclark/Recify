{-# LANGUAGE OverloadedStrings #-}

module Writers.HTML where

import qualified Types.RecentlyPlayedWithArtist as RPWA
import Data.List
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as DTIO
import Web.Scotty
import Control.Monad.IO.Class

import Writers.CSV
import Utils.String

getRecentlyPlayedHTMLResponse :: RPWA.RecentlyPlayedWithArtist -> IO LT.Text
getRecentlyPlayedHTMLResponse recentlyPlayed = do
  _ <- writeCsvToDisk recentlyPlayed
  return . stringToLazyText . buildRecentlyPlayedHTMLList . RPWA.tracks . RPWA.recentlyPlayed $ recentlyPlayed

getNextRecentlyPlayedTracksHref :: RPWA.RecentlyPlayedWithArtist -> LT.Text
getNextRecentlyPlayedTracksHref = stringToLazyText . RPWA.next

getArtistsFromTrack :: RPWA.Track -> String
getArtistsFromTrack track = concat . intersperse ", " $ fmap (\artist -> ((RPWA.artistName artist) ++ " - " ++ (intercalate ", " $ RPWA.genres artist))) (RPWA.artists track)

buildRecentlyPlayedHTMLList :: [RPWA.Track] -> String
buildRecentlyPlayedHTMLList [] = []
buildRecentlyPlayedHTMLList tracks = concat . intersperse "<br>" $ fmap (\track -> "<li>" ++ (RPWA.name track) ++ " - " ++ (Writers.HTML.getArtistsFromTrack track) ++ "</li>") tracks

buildResponse :: MonadIO m => LT.Text -> LT.Text -> m LT.Text
buildResponse recentlyPlayedHTML nextHTML = do
        liftIO $ putStrLn "Parsing Dashboard HTML"
        dashboardHtml <- liftIO . DTIO.readFile $ "./static/dashboard.html"
        return $ LT.replace "{{nextHTML}}" nextHTML (LT.replace "{{recentlyPlayedHTML}}" recentlyPlayedHTML $ LT.fromStrict dashboardHtml)
