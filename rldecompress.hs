import Data.Char

-- Nodig voor de IO
import System.Environment
import System.IO

main = do
    -- Lees het input en output bestand
    args <- getArgs
    let inputFile = args!!0
    let outputFile = args!!1

    -- Lees de gecomprimeerde tekst
    compressedText <- readFile inputFile

    -- Decomprimeer de tekst
    let originalText = rldecompress compressedText

    -- Geef de lengte van de outputfile
    let lengthOfOutputFile = length originalText
    putStrLn("length of decompressed file " ++ outputFile ++ ": " ++ (show lengthOfOutputFile) ++ " characters.")

    -- Schrijf de originele tekst naar het eerder opgegeven output bestand
    writeFile outputFile originalText

    putStrLn "done..."

----------------------------------------------

-- Decomprimeer een string volgens het run-length algoritme
-- 1. converteer de string naar onze typecast   [(3,'a'),(2,'b'),(1,'c'),(3,'d')]
-- 2. gerbuik concatMap en replicate            ["aaabbcddd"]
--    om een string te maken

-- Typecasting!
-- Dit type is een lijst van combinaties van letter en het aantal
type RLEntry a = [(Int, a)]

-- concatMap hebben we behandeld: deze voert op een lijst steeds dezelfde functie uit
-- uncurry is wel nieuw, deze voert een curried functie uit op een paar
-- Zie Hoogle voor voorbeelden: https://hackage.haskell.org/package/hspec-2.7.1/docs/Test-Hspec-Discover.html#v:uncurry
-- replicate neemt een argument en print het tweede argument zo vaak
-- replicate 5 'a' = "aaaaa"
rldecompress :: String -> String
rldecompress x = concatMap (uncurry replicate) $ splitString x

-- We behandelen de string natuurlijk als lijst van karakters
-- We kijken alleen naar de eerste waarde en controleren of dit een getal is
splitString :: String -> RLEntry Char
splitString "" = []
splitString (s:ss) = if isDigit s
                        -- Als dit zo is, splitsen we de lijst op
                        then let (n,c:cs) = span isDigit (s:ss)
                            -- read cast een string naar een ander type
                            -- Zo wordt een string toch een integer, noodzakelijk voor RLE
                            -- Vervolgens gaan we verder met de rest
                            in (read n, c):(splitString cs)
                        -- Als er geen getal is, is er slechts 1
                        -- TODO: er is een else nodig
                        -- Ik heb liever when, maar dat werkt niet bij mij
                        else (1, s):(splitString ss)