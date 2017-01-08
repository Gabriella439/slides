-- exercises/07/Main.hs

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

data Nomen = Nomen
    { genus    :: Text
    , subgenus :: Text
    } deriving (Show)

instance FromNamedRecord Nomen where
    parseNamedRecord m = do
        genus    <- m .: "Genus.current"
        subgenus <- m .: "Subgenus.current"
        return (Nomen {..})

process :: FilePath -> IO (Vector Nomen)
process file = do
    bytes <- Data.ByteString.Lazy.readFile file
    case Data.Csv.decodeByName bytes of
        Left   err        -> fail err
        Right (_, vector) -> return vector

main :: IO ()
main = do
    nomina <- process "holothuriidae-nomina-valid.csv"

    let hasSubgenus :: Nomen -> Bool
        hasSubgenus (Nomen {..}) = subgenus /= ""

    let genera :: Vector Text
        genera = fmap genus (Data.Vector.filter hasSubgenus nomina)

    print (Data.List.nub (toList genera))
    -- ["Holothuria"]
