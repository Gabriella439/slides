-- exercises/00/Main.hs

import Data.ByteString.Lazy (ByteString)

import qualified Data.ByteString.Lazy.Char8

preview :: ByteString -> ByteString
preview =
      Data.ByteString.Lazy.Char8.unlines
    . take 3
    . Data.ByteString.Lazy.Char8.lines

process :: FilePath -> IO ()
process file = do
    bytes <- Data.ByteString.Lazy.Char8.readFile file
    Data.ByteString.Lazy.Char8.putStr (preview bytes)

main :: IO ()
main = do
    process "holothuriidae-specimens.csv"
    putStrLn ""
    process "holothuriidae-nomina-valid.csv"
