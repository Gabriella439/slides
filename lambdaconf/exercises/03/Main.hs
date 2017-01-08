-- exercises/03/Main.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Data.Csv (FromNamedRecord(..), (.:))
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.List

data Specimen = Specimen
    { institutionCode :: Text
    } deriving (Show)

instance FromNamedRecord Specimen where
    parseNamedRecord m = do
        institutionCode <- m .: "dwc:institutionCode"
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
    let institutions :: Vector Text
        institutions = fmap institutionCode specimens
    print (Data.List.nub (toList institutions))
    -- ["FLMNH","CAS","MCZ","YPM"]
