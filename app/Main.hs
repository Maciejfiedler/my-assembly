module Main where
import Data.Either
import qualified Data.Map as M

main :: IO ()
main = do
    instructionFile <- readFile "instruction.brb" :: IO String
    let instructionLines = lines instructionFile :: [String]
    let results = map runInstruction instructionLines
    print results

runInstruction :: String -> String
runInstruction str =
    let instructionInTrippleTuple = convertStringToTrippleTuple str
    in case instructionInTrippleTuple of
        Right (xString,yString,zString) -> 
            let funcMaybe = seekFuncName xString
            in case funcMaybe of
                Just func -> 
                    let y = read yString :: Double
                        z = read zString :: Double
                        in show $ func y z 
                Nothing -> "failure"
        Left x -> x

        -- TODO: handle either case

-- turn the string into a tuple where tuple = (function, arg1, arg2)
convertStringToTrippleTuple :: String -> Either String (String, String, String)
convertStringToTrippleTuple xs = let ys = words xs in if length ys >= 3 then
                                let (x:y:z:_) = ys in Right (x,y,z)
                                else Left "instruction has too few arguments"

-- convert the function from the instruction into a haskell function
seekFuncName :: (Fractional a) => String -> Maybe (a -> a -> a) 
seekFuncName x = M.lookup x funcNameMap

-- list where the instruction function is bound to a haskell function
funcNameMap :: (Fractional a) => M.Map String (a -> a -> a)
funcNameMap = M.fromList [
    ("ADD", (+)),
    ("MUL", (*)),
    ("SUB", (-)),
    ("DIV", (/))]