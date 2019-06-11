{-# LANGUAGE OverloadedStrings #-}

module Writers.CSV where

import Data.List

import qualified Types.RecentlyPlayedWithArtist as RPWA

boolToString :: Bool -> String
boolToString True = "1"
boolToString False = "0"

data Field = Field
  { field :: String
  } deriving (Show)

data Row = Row
  { row :: [Field]
  } deriving (Show)

data CSV = CSV
  { csv :: [Row]
  }

fieldsToString :: Field -> String
fieldsToString = field

seperateFieldsByDelimiter :: Row -> [Char]
seperateFieldsByDelimiter = concat . intersperse "," . fmap fieldsToString . row

seperateRowsByDelimiter :: [Row] -> [Char]
seperateRowsByDelimiter = concat . intersperse "\n" . fmap seperateFieldsByDelimiter

getArtistsFromTrack :: RPWA.Track -> String
getArtistsFromTrack track = concat . intersperse " | " $ fmap (\artist -> RPWA.artistName artist) (RPWA.artists track)

createRowsFromTracks :: [RPWA.Track] -> [Row]
createRowsFromTracks tracks = (Row [
    Field $ "title",
    Field $ "artist",
    Field $ "explicit"
  ]):fmap (\track ->
    Row [
        Field . RPWA.name $ track,
        Field . getArtistsFromTrack $ track,
        Field . boolToString . RPWA.explicit $ track
        ]
      ) tracks

createCSV :: RPWA.RecentlyPlayedWithArtist -> CSV
createCSV recentlyPlayed = CSV . createRowsFromTracks . RPWA.tracks . RPWA.recentlyPlayed $ recentlyPlayed

writeCsvToDisk :: RPWA.RecentlyPlayedWithArtist -> IO ()
writeCsvToDisk = writeFile "./recentlyPlayed.csv" . seperateRowsByDelimiter . csv . createCSV
