{-# LANGUAGE OverloadedStrings #-}

module Writers.CSV where

import Data.List

import qualified Types.RecentlyPlayed as RP

boolToString :: Bool -> String
boolToString True = "1"
boolToString False = "0"

data Field = Field { 
  field :: String 
} deriving (Show)

data Row = Row {
    row :: [Field]
} deriving (Show)

data CSV = CSV {
    csv :: [Row]
}

fieldsToString :: Field -> String
fieldsToString fields = field $ fields

seperateFieldsByDelimiter :: Row -> [Char]
seperateFieldsByDelimiter rows = concat . intersperse "," . fmap fieldsToString . row $ rows

seperateRowsByDelimiter :: [Row] -> [Char]
seperateRowsByDelimiter rows = concat . intersperse "\n" . fmap seperateFieldsByDelimiter $ rows

getArtistsFromTrack :: RP.Track -> String
getArtistsFromTrack track = concat . intersperse " | " $ fmap (\artist -> (RP.artistName artist)) (RP.artists track)

getArtistIdFromTrack :: RP.Track -> String
getArtistIdFromTrack track = concat $ fmap (\artist -> (RP.id artist)) (RP.artists track)

createCSV :: RP.RecentlyPlayed -> CSV
createCSV recentlyPlayed = CSV . fmap (\track -> 
    Row [
        Field . getArtistIdFromTrack $ track,
        Field . RP.name $ track,
        Field . getArtistsFromTrack $ track,
        Field . boolToString . RP.explicit $ track
        ]
    ) . RP.tracks . RP.recentlyPlayed $ recentlyPlayed

writeCsvToDisk :: RP.RecentlyPlayed -> IO ()
writeCsvToDisk recentlyPlayed = writeFile "./recentlyPlayed.csv" . seperateRowsByDelimiter . csv . createCSV $ recentlyPlayed
