module Main where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
    bsFile <- B.readFile "instruction.brb" :: IO B.ByteString
    print $ stringElem "ADD" bsFile

stringElem :: String -> B.ByteString -> [Bool]
stringElem (x:xs) bs = B.elem x (B.head bs) : stringElem xs (B.tail bs)