module Main where

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
runInstruction = id