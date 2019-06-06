{-# LANGUAGE OverloadedStrings #-}

module Writers.CSV where

import Data.List

import qualified Types.RecentlyPlayedWithArtist as RPWA

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

getArtistsFromTrack :: RPWA.Track -> String
getArtistsFromTrack track = concat . intersperse " | " $ fmap (\artist -> (RPWA.artistName artist)) (RPWA.artists track)

createCSV :: RPWA.RecentlyPlayedWithArtist -> CSV
createCSV recentlyPlayed = CSV . fmap (\track -> 
    Row [
        Field . RPWA.name $ track,
        Field . getArtistsFromTrack $ track,
        Field . boolToString . RPWA.explicit $ track
        ]
    ) . RPWA.tracks . RPWA.recentlyPlayed $ recentlyPlayed

writeCsvToDisk :: RPWA.RecentlyPlayedWithArtist -> IO ()
writeCsvToDisk recentlyPlayed = writeFile "./recentlyPlayed.csv" . seperateRowsByDelimiter . csv . createCSV $ recentlyPlayed
