-- exercises/01.hs

import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.Vector

process :: FilePath -> IO ()
process file = do
    bytes <- Data.ByteString.Lazy.readFile file
    case Data.Csv.decodeByName bytes of
        Left err -> do
            fail err
        Right (header, vector) -> do
            print header
            let _ = vector :: Vector (Map Text Text)
            mapM_ print (Data.Vector.take 2 vector)

main :: IO ()
main = do
    process "holothuriidae-specimens.csv"
    putStrLn ""
    process "holothuriidae-nomina-valid.csv"
