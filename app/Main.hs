module Main where
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Bu

main :: IO ()
main = do
    bsFile <- B.readFile "instruction.brb" :: IO B.ByteString
    B.putStr bsFile