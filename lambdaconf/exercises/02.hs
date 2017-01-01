-- exercises/02.hs

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
    print (length specimens)
    -- 2984
