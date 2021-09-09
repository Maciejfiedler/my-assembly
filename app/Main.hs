module Main where
import Data.Either
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map as M

type Arg = B.ByteString
type Func = B.ByteString
type Result = B.ByteString 
type Expr = (Func, Arg, Arg)
--type Function = (Int -> Int . )

main :: IO ()
main = do
    bsFile <- B.readFile "instruction.brb" :: IO B.ByteString
    let bsLines = B8.lines bsFile
    let bsInWords = B8.words $ head bsLines -- run only first line
    let result = runFunction bsInWords
    B.putStr result

runFunction :: [B.ByteString] -> B.ByteString
runFunction bs = either B8.pack processFunction exprEither
    where 
        exprEither = seekExpr bs

processFunction :: Expr -> B.ByteString -- needs more work
processFunction (func, a, b) = 
    case haskellFunc of 
        Just actualFunc -> B8.pack $ show $ (+) arg1 arg2 
        Nothing -> (B8.pack "couldn't find function")
    where
        haskellFunc = M.lookup func exprList
        arg1 = read $ B8.unpack a :: Int
        arg2 = read $ B8.unpack b :: Int


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
            ("ADD", "(+)")] :: [(String,String)]
