import System.IO  
import System.Directory
import System.Environment
import Data.List

main = do
    -- get the parameters 
    args <- getArgs
    let inputFile = args!!0
    let outputFile = args!!1

    -- read and compress the input text 
    inputText <- readFile inputFile;
    let outputText = rlcompress inputText
    
    -- get the length of the input and output
    let orginalLength = length inputText
    let compressedLength = length outputText
    putStrLn("length of " ++ inputFile ++ ": " ++ (show orginalLength) ++ " characters")
    putStrLn("length of compressed file " ++ outputFile ++ ": " ++ (show compressedLength) ++ " characters")

    -- calculate and print compression factor
    let compressionFactor = round $ ((fromIntegral compressedLength) / (fromIntegral orginalLength)) * 100
    putStrLn("factor: " ++ (show compressedLength) ++ "/" ++ (show orginalLength) ++ "*100=" ++ (show compressionFactor) ++ "%")

    -- write the compressed string to the previously specified output file
    writeFile outputFile outputText
    putStrLn("done...")

----------------------------------------------

-- compresses a string according to the run-length algorithm
-- 1. group the string              ["aaa", "bb", "c", "ddd"]
-- 2. compresses each group         ["3a", "2b", "c", "3d"])
-- 3. use concat to make one string ["3a2bc3d"]
rlcompress :: String -> String
rlcompress x = concat $ map rlcompressString $ group x

-- compresses a string with the same characters
rlcompressString :: String -> String
rlcompressString x
    | length x>1  = (show . length $ x) ++ [head x]
    | otherwise   = [head x]