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
  -- _ <- writeCsvToDisk recentlyPlayed
  return . stringToLazyText . buildRecentlyPlayedHTMLList . RPWA.tracks . RPWA.recentlyPlayed $ recentlyPlayed

getNextRecentlyPlayedTracksHref :: RPWA.RecentlyPlayedWithArtist -> LT.Text
getNextRecentlyPlayedTracksHref = stringToLazyText . RPWA.next

getArtistNames artists = intercalate ", " $ fmap RPWA.artistName artists

getArtistGenres artists = intercalate ", " . concat $ fmap RPWA.genres artists

getArtistsFromTrack :: RPWA.Track -> String
getArtistsFromTrack track = "<td>" ++ (getArtistNames (RPWA.artists track)) ++ "</td><td>" ++ (getArtistGenres (RPWA.artists track)) ++ "</td>"

buildRecentlyPlayedHTMLList :: [RPWA.Track] -> String
buildRecentlyPlayedHTMLList [] = []
buildRecentlyPlayedHTMLList tracks = concat $ fmap (\track -> "<tr><td>" ++ (RPWA.name track) ++ "</td>" ++ (Writers.HTML.getArtistsFromTrack track) ++ "</tr>") tracks

buildResponse :: MonadIO m => LT.Text -> LT.Text -> m LT.Text
buildResponse recentlyPlayedHTML nextHTML = do
        dashboardHtml <- liftIO . DTIO.readFile $ "./static/dashboard.html"
        return $ LT.replace "{{nextHTML}}" nextHTML (LT.replace "{{recentlyPlayedHTML}}" recentlyPlayedHTML $ LT.fromStrict dashboardHtml)
