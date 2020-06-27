import Huffman

import qualified Data.Map as M

-- Nodig voor de IO
import System.Environment
import System.IO

-- Encode een string met een codemap
-- TODO: integreer het aanmaken van de codemap hierin
encode :: Bit a => Codemap a -> String -> [a]
encode m = concatMap (m M.!)

main = do
    -- Lees het input en output bestand
    args <- getArgs
    let inputFile = (head args)
    let outputFile = args!!1

    -- De file waar de tree naartoe wordt geschreven
    let treeFile = args!!2

    -- Lees de input tekst
    originalText <- readFile inputFile

    -- Geef de originele lengte weer
    let lengthOfInputFile = length originalText
    putStrLn("length of " ++ inputFile ++ ": " ++ (show lengthOfInputFile) ++ " characters, " ++ (show $ lengthOfInputFile * 8) ++ " Bit.")

    -- Comprimeer de string 
    let compressedText = encode (stringCodemap originalText) originalText
    
    -- Geef de lengte van de outputfile en de compressie
    let lengthOfOutputFile = length compressedText
    putStrLn("length of compressed file " ++ outputFile ++ ": " ++ (show $ lengthOfOutputFile `div` 8) ++ " characters, " ++ (show lengthOfOutputFile) ++ " bits.")

    -- Bereken en print de compressiefactor
    let factor = round $ ((fromIntegral $ lengthOfOutputFile `div` 8) / (fromIntegral lengthOfInputFile)) * 100
    putStrLn("factor: " ++ (show $ lengthOfOutputFile `div` 8) ++ " / " ++ (show lengthOfInputFile) ++ " * 100% = " ++ (show factor) ++ "%")

    -- Schrijf de gecomprimeerde tekst en de boom naar de eerder opgegeven output bestanden
    writeFile outputFile $ concatMap show (compressedText :: [Int])
    writeFile treeFile $ show $ stringTree originalText

    putStrLn "done..."
