import Data.Char
import System.Environment

main = do
    -- get the parameters 
    args <- getArgs
    let inputFile = args!!0
    let outputFile = args!!1

    -- read the compressed text
    compressedText <- readFile inputFile

    -- decompress the text
    let originalText = rldecompress compressedText

    -- write the original text to the previously specified output file
    writeFile outputFile originalText

-------------------

-- decompresses a string according to the run-length algorithm
-- 1. group the string              ["3a", "2b", "c", "3d"]
-- 2. decompress each group         ["aaa", "bb", "c", "ddd"]
-- 3. use concat to make one string ["aaabbcddd"]

type RLEntry a = [(Int, a)]

rldecompress :: String -> String
-- rldecompress x = concat $ map (rldecompressEntry) $ splitInput x
rldecompress x = rldecompressEntry $ splitString x

rldecompressEntry :: RLEntry a -> [a]
rldecompressEntry = concatMap (uncurry replicate)

splitString :: String -> RLEntry Char
splitString "" = []
splitString (s:ss) = if isDigit s
                  then let (n,c:cs) = span isDigit (s:ss)
                       in (read n, c):(splitString cs)
                  else (1, s):(splitString ss)