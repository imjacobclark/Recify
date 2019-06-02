{-# LANGUAGE OverloadedStrings #-}

module CSVWriter where

import Data.List

import qualified RecentlyPlayedMarshall as RPM

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

createCSV :: RPM.RecentlyPlayed -> CSV
createCSV recentlyPlayed = CSV . fmap (\track -> 
    Row [
        Field . RPM.name $ track,
        Field . boolToString . RPM.explicit $ track
        ]
    ) . RPM.tracks . RPM.recentlyPlayed $ recentlyPlayed

writeCsvToDisk :: RPM.RecentlyPlayed -> IO ()
writeCsvToDisk recentlyPlayed = writeFile "./recentlyPlayed.csv" . seperateRowsByDelimiter . csv . createCSV $ recentlyPlayed
