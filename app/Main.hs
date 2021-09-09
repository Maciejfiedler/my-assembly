module Main where
import Data.Either

main :: IO ()
main = do
    instructionFile <- readFile "instruction.brb" :: IO String
    let instructionLines = lines instructionFile :: [String] 
    let results = runInstructions instructionLines
    print results

runInstructions :: [String] -> [String]
runInstructions [] = []
runInstructions (x:xs) = (runInstruction x):(runInstructions xs)

runInstruction :: String -> String
runInstruction str = str
    where
        instructionInTrippleTuple = convertStringToTrippleTuple str -- TODO: handle either case

-- turn the string into a tuple where tuple = (function, arg1, arg2)
convertStringToTrippleTuple :: String -> Either String (String, String, String)
convertStringToTrippleTuple xs = let ys = words xs in if length ys >= 3 then
                                let (x:y:z:_) = ys in Right (x,y,z)
                                else Left "instruction has too few arguments"