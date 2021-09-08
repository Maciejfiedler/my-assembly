module Main where
import Data.Either
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

main :: IO ()
main = do
    bsFile <- B.readFile "instruction.brb" :: IO B.ByteString
    print $ getExpression bsFile

type Expression = (String, String, String) -- function, 1 argument, 2 argument

getExpression :: B.ByteString -> Either String Expression
getExpression bs = 
    let bytestrings = B8.words bs
        strings = bytestringsToStrings bytestrings
        in if((length strings) >= 3) then
            let (func:a:b:_) = strings
            in Right (func, a, b)
            else
                Left "too few arguments"
        
bytestringsToStrings :: [B.ByteString] -> [String]
bytestringsToStrings [] = []
bytestringsToStrings (x:xs) = (B8.unpack x):bytestringsToStrings xs