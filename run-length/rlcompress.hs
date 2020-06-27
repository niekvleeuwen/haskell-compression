import Data.List

-- Nodig voor de IO
import System.Environment
import System.IO

main = do
    -- Lees het input en output bestand
    args <- getArgs
    let inputFile = args!!0
    let outputFile = args!!1

    -- Lees de input tekst
    originalText <- readFile inputFile

    -- Comprimeer de ingelezen tekst
    let compressedText = rlcompress originalText
    
    -- Geef de lengte van het input en output bestand
    let orginalLength = length originalText
    let compressedLength = length compressedText
    putStrLn("length of " ++ inputFile ++ ": " ++ (show orginalLength) ++ " characters")
    putStrLn("length of compressed file " ++ outputFile ++ ": " ++ (show compressedLength) ++ " characters")

    -- Bereken en print de compressiefactor
    let compressionFactor = round $ ((fromIntegral compressedLength) / (fromIntegral orginalLength)) * 100
    putStrLn("factor: " ++ (show compressedLength) ++ "/" ++ (show orginalLength) ++ "*100=" ++ (show compressionFactor) ++ "%")

    -- Schrijf de gecomprimeerde tekst naar het eerder opgegeven output bestand
    writeFile outputFile compressedText
    putStrLn("done...")

----------------------------------------------

-- Comprimeer een string volgens het run-length algoritme
-- 1. groepeer de string                    ["aaa", "bb", "c", "ddd"]
-- 2. comprimeer elke groep                 ["3a", "2b", "c", "3d"]
-- 3. maak er een string van met concatMap  ["3a2bc3d"]
rlcompress :: String -> String
rlcompress x = concatMap rlcompressEntry $ group x

-- Comprimeert een string met dezelfde karakters
rlcompressEntry :: String -> String
rlcompressEntry x
    | length x>1 = (show $ length x) ++ [head x]
    | otherwise  = [head x]