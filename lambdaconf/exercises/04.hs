-- exercises/04.hs

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

import Data.Csv (FromNamedRecord(..), (.:))
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Diagrams
import Diagrams.Prelude (blue)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.List
import qualified Data.List.NonEmpty
import qualified Data.Text
import qualified Diagrams.Backend.SVG (renderSVG)

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

chart :: [(Text, Double)] -> IO ()
chart xys = do
    let maxY = maximum [ y | (_, y) <- xys ]

    let bar (_, y) = alignB (fc blue (rect 10 (10 * y / maxY)))
    let bars = hcat (map bar xys)

    let label (x, _) = alignT (text (Data.Text.unpack x) <> rect 10 2)
    let labels = hcat (map label xys)

    let yMax = alignR (hrule 1) <> alignL (text (show maxY) <> rect 4 2)
    let yMin = alignR (hrule 1) <> alignL (text  "0"        <> rect 4 2)
    let yAxis0 = alignT (vrule 10) <> alignL yMax
    let yAxis  = alignBL yAxis0 <> alignL yMin

    let diagram = yAxis <> alignBR bars <> alignTR labels

    Diagrams.Backend.SVG.renderSVG "chart.svg" (dims (V2 512 256)) diagram

main :: IO ()
main = do
    specimens <- process "holothuriidae-specimens.csv"
    let institutions = toList (fmap institutionCode specimens)
    let toBar xs = (Data.List.NonEmpty.head xs, fromIntegral (length xs))
    chart (map toBar (Data.List.NonEmpty.group (Data.List.sort institutions)))
