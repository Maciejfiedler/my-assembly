module Main where
import Data.Either
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map as M

type Arg = B.ByteString
type Func = B.ByteString
type Result = B.ByteString 
type Expr = (Func, Arg, Arg)

main :: IO ()
main = do
    bsFile <- B.readFile "instruction.brb" :: IO B.ByteString
    let bsLines = B8.lines bsFile
    let bsInWords = B8.words $ head bsLines -- first line
    let result = runFunction bsInWords
    B.putStr result

runFunction :: [B.ByteString] -> B.ByteString
runFunction bs = 
    where 
        expr = seekExpr bs

seekExpr :: [B.ByteString] -> Either String Expr
seekExpr bs =
    if length bs >= 3 then
        Right (func,a,b)
    else
        Left "too few arguments"
    where (func:a:b:_) = bs

exprList :: M.Map B.ByteString  B.ByteString 
exprList = M.fromList exprInTuple

exprInTuple :: [(B.ByteString, B.ByteString)]
exprInTuple = map (\(x,y) -> (B8.pack x, B8.pack y)) list
    where 
        list = [
            ("ADD", "add")] :: [(String,String)]
