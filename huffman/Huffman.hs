module Huffman where

    import qualified Data.Map as M
    import Control.Arrow
    import Data.List
    import Data.Function

    class Eq a => Bit a where
        zer :: a
        one :: a

    instance Bit Int where
        zer = 0
        one = 1

    type Codemap a = M.Map Char [a]

    -- De HuffmanTree bestaat uit Leaves en Forks,
    -- die op zichzelf ook weer bestaan uit "HuffmanTrees"
    data HuffmanTree = Leaf Char Int
                     | Fork HuffmanTree HuffmanTree Int
                     deriving (Show,Read)
                    
    -- Deze gestandaardiseerde manier maakt het mogelijk om de weight te krijgen,
    -- ongeacht het type HuffmanTree
    weight :: HuffmanTree -> Int
    weight (Leaf _ w)    = w
    weight (Fork _ _ w)  = w

    -- Het mergen van twee HuffmanTrees behoudt ook de originele componenten,
    -- door te mergen hoef je dus niet de leaves apart toe te voegen
    merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)

    -- TODO, FIXME: sorteren kan ook prima in deze frequency list,
    -- daarnaast zijn er nettere en geschiktere methodes
    freqList :: String -> [(Char, Int)]
    freqList = M.toList . M.fromListWith (+) . map (flip (,) 1)

    -- Deze functie bouwt recursief de tree op
    -- Hierbij is de list uit freqList de input voor deze functie
    buildTree :: [(Char, Int)] -> HuffmanTree
    buildTree = bld . map (uncurry Leaf) . sortBy (compare `on` snd)
        where   bld (t:[]) = t
                bld (a:b:cs)  = bld $ insertBy (compare `on` weight) (merge a b) cs

    -- Zodra de tree is opgebouwd, moet er natuurlijk nog doorheen worden genavigeerd
    -- Hierbij worden de Bit geset
    buildCodemap :: Bit a => HuffmanTree -> Codemap a
    buildCodemap = M.fromList . buildCodelist
        where   buildCodelist (Leaf c w)    = [(c, [])]
                buildCodelist (Fork l r w)  = map (addBit zer) (buildCodelist l) ++ map (addBit one) (buildCodelist r)
                    where addBit b = second (b :)

    -- Deze functie is de core van de compression
    -- Het converteert een string naar een Huffman tree
    stringTree :: String -> HuffmanTree
    stringTree = buildTree . freqList

    -- Een wrapper die een string omzet naar een codemap via buildCodemap
    stringCodemap :: Bit a => String -> Codemap a
    stringCodemap = buildCodemap . stringTree