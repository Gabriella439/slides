-- exercises/07.hs

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy
import qualified Data.Csv

process :: FilePath -> IO (Vector (Map Text Text))
process file = do
    bytes <- Data.ByteString.Lazy.readFile file
    case Data.Csv.decodeByName bytes of
        Left   err        -> fail err
        Right (_, vector) -> return vector

main :: IO ()
main = do
    specimens <- process "holothuriidae-specimens.csv"

    print (lengthOf (traverse . ix "dwc:class" . only "") specimens)
    -- 50

    print (lengthOf (traverse . ix "dwc:class" . only "Holothuroidea") specimens)
    -- 2934

    let modify c = if c == "" then "Holothuroidea" else c
    let specimens2 = over (traverse . ix "dwc:class") modify specimens

    print (lengthOf (traverse . ix "dwc:class" . only "") specimens2)
    -- 0

    print (lengthOf (traverse . ix "dwc:class" . only "Holothuroidea") specimens2)
    -- 2984
