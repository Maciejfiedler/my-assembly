module Main where
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

main :: IO ()
main = do
    bsFile <- B.readFile "instruction.brb" :: IO B.ByteString
    print $ getExpression bsFile

type Expression = (String, String, String) -- function, 1 argument, 2 argument

getExpression :: B.ByteString -> Expression
getExpression bs = (func, a, b)
    where 
        bsInWords = B8.words bs :: [B.ByteString]
        (func:a:b:_) = bytestringListToString bsInWords
        
bytestringListToString :: [B.ByteString] -> [String]
bytestringListToString [] = []
bytestringListToString (x:xs) = (B8.unpack x):bytestringListToString xs