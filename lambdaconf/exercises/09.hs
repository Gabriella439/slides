-- exercises/09.hs

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

import Data.Csv
    (DefaultOrdered(..), FromNamedRecord(..), ToNamedRecord(..), (.:), (.=))
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.Discrimination
import qualified Data.Discrimination.Grouping
import qualified Data.Text
import qualified Data.Vector

data Specimen = Specimen
    { institutionCode :: Text
    , uuid            :: Text
    , catalogNumber   :: Text
    , specificEpithet :: Text
    , genus           :: Text
    } deriving (Show)

instance FromNamedRecord Specimen where
    parseNamedRecord m = do
        institutionCode <- m .: "dwc:institutionCode"
        uuid            <- m .: "idigbio:uuid"
        catalogNumber   <- m .: "dwc:catalogNumber"
        specificEpithet <- m .: "dwc:specificEpithet"
        genus           <- m .: "dwc:genus"
        return (Specimen {..})

instance ToNamedRecord Specimen where
    toNamedRecord (Specimen {..}) =
        Data.Csv.namedRecord
            [ "dwc:institutionCode" .= institutionCode
            , "idigbio:uuid"        .= uuid
            , "dwc:catalogNumber"   .= catalogNumber
            , "dwc:specificEpithet" .= specificEpithet
            , "dwc:genus"           .= genus
            ]

instance DefaultOrdered Specimen where
    headerOrder _ =
         Data.Vector.fromList
             [ "dwc:institutionCode"
             , "idigbio:uuid"
             , "dwc:catalogNumber"
             , "dwc:specificEpithet"
             , "dwc:genus"
             ]

data Nomina = Nomina
    { status  :: Text
    , genus   :: Text
    , species :: Text
    } deriving (Show)

instance FromNamedRecord Nomina where
    parseNamedRecord m = do
        status  <- m .: "Status"
        genus   <- m .: "Genus.current"
        species <- m .: "species.current"
        return (Nomina {..})

process :: FromNamedRecord a => FilePath -> IO (Vector a)
process file = do
    bytes <- Data.ByteString.Lazy.readFile file
    case Data.Csv.decodeByName bytes of
        Left   err        -> fail err
        Right (_, vector) -> return vector

main :: IO ()
main = do
    specimens <- process "holothuriidae-specimens.csv"
    nomina    <- process "holothuriidae-nomina-valid.csv"

    let leftKey  (Specimen {..}) = Data.Text.unwords [genus, specificEpithet]
    let rightKey (Nomina   {..}) = Data.Text.unwords [genus, species        ]

    let joinedGroups =
            Data.Discrimination.leftOuter
                Data.Discrimination.Grouping.hashing
                (\specimen (Nomina {..}) -> (specimen, Just status))
                (\specimen -> (specimen, Nothing))
                leftKey
                rightKey
                (toList specimens)
                (toList nomina   )

    let joined               = concat joinedGroups
    let pending (_, status)  = status /= Just "accepted"
    let select (specimen, _) = specimen
    let output               = map select (filter pending joined)

    let bytes = Data.Csv.encodeDefaultOrderedByName output
    Data.ByteString.Lazy.writeFile "holothuriidae-pending.csv" bytes
