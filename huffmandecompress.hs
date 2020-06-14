import Huffman

import Data.Char

-- Nodig voor de IO
import System.Environment
import System.IO

-- Decomprimeer een lijst met bits met behulp van de huffman boom
decode :: Bit a => HuffmanTree -> [a] -> String
decode tree = dcd tree
    where  dcd (Leaf c _) []        = [c]
           dcd (Leaf c _) bs        = c : dcd tree bs
           dcd (Fork l r _) (b:bs)  = dcd (if b == zer then l else r) bs

-- Converteer integers naar bits
convertToBits :: Bit a => [Int] -> [a]
convertToBits cs = map (\x -> if x == 0 then zer else one) cs

main = do
    -- Lees het input en output bestand
    args <- getArgs
    let inputFile = args!!0
    let outputFile = args!!1

    -- De file waar de tree vanaf wordt gelezen
    let treeFile = args!!2

    -- Lees de input tekst
    originalText <- readFile inputFile

    -- Converteer de input tekst naar een list met bits
    inputText <- readFile inputFile
    let compressedText = convertToBits $ map digitToInt inputText

    -- Lees de huffman boom in
    huffmanTreeText <- readFile treeFile
    let huffmanTree = read huffmanTreeText :: HuffmanTree

    -- Decomprimeer de bits met behulp van de boom
    let result = decode huffmanTree (compressedText :: [Int])
    
    -- Geef de lengte van de outputfile
    let lengthOfOutputFile = length result
    putStrLn("length of decompressed file " ++ outputFile ++ ": " ++ (show lengthOfOutputFile) ++ " characters, " ++ (show $ lengthOfOutputFile * 8) ++ " Bit.")

    -- Schrijf de originele tekst naar het eerder opgegeven output bestand
    writeFile outputFile result

    putStrLn "done..."
