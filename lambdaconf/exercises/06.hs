-- exercises/06.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Data.Csv (FromNamedRecord(..), (.:))
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.List
import qualified Data.Vector

data Specimen = Specimen
    { institutionCode :: Text
    , year            :: Maybe Integer
    } deriving (Show)

instance FromNamedRecord Specimen where
    parseNamedRecord m = do
        institutionCode <- m .: "dwc:institutionCode"
        year            <- m .: "dwc:year"
        return (Specimen {..})

process :: FilePath -> IO (Vector Specimen)
process file = do
    bytes <- Data.ByteString.Lazy.readFile file
    case Data.Csv.decodeByName bytes of
        Left   err        -> fail err
        Right (_, vector) -> return vector

main :: IO ()
main = do
    specimens <- process "holothuriidae-specimens.csv"

    let years = do
            specimen <- toList specimens
            Just y   <- return (year specimen)
            return y

    print (take 10 (Data.List.sort years))
    -- [1,91,91,91,91,91,91,1902,1902,1957]

    print (Data.Vector.minimum (filter (1700 <) years))
    -- Just 1902

    let inRange year = 2006 <= year && year <= 2014
    let matches = Data.Vector.filter inRange years
    print (fromIntegral (length matches) / fromIntegral (length specimens) :: Double)
    -- 0.4932975871313673
