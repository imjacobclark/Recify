{-# LANGUAGE OverloadedStrings #-}

module Writers.HTML where

import qualified Types.RecentlyPlayed as RP
import Data.List
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as DTIO
import Web.Scotty
import Control.Monad.IO.Class

import Writers.CSV
import Utils.String

getRecentlyPlayedHTMLResponse :: RP.RecentlyPlayed -> IO LT.Text
getRecentlyPlayedHTMLResponse recentlyPlayed = do
  _ <- writeCsvToDisk recentlyPlayed
  return (stringToLazyText . buildRecentlyPlayedHTMLList . RP.tracks . RP.recentlyPlayed $ recentlyPlayed)

getNextRecentlyPlayedTracksHref :: RP.RecentlyPlayed -> LT.Text
getNextRecentlyPlayedTracksHref recentlyPlayed = stringToLazyText . RP.next $ recentlyPlayed

getArtistsFromTrack :: RP.Track -> String
getArtistsFromTrack track = concat . intersperse ", " $ fmap (\artist -> (RP.artistName artist)) (RP.artists track)

buildRecentlyPlayedHTMLList :: [RP.Track] -> String
buildRecentlyPlayedHTMLList [] = []
buildRecentlyPlayedHTMLList (tracks) = concat . intersperse "<br>" $ fmap (\track -> "<li>" ++ (RP.name track) ++ " - " ++ (Writers.HTML.getArtistsFromTrack track) ++ "</li>") tracks

buildResponse :: LT.Text -> LT.Text -> ActionM ()
buildResponse recentlyPlayedHTML nextHTML = do
        dashboardHtml <- (liftIO $ DTIO.readFile "./static/dashboard.html")
        html $ mconcat [
          LT.replace "{{nextHTML}}" nextHTML (LT.replace "{{recentlyPlayedHTML}}" recentlyPlayedHTML (LT.fromStrict dashboardHtml))
          ]  