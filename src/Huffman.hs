module Huffman where

data HuffmanTree a = Leaf a Int
    | Node (HuffmanTree a) (HuffmanTree a) Int
    deriving (Show, Eq)

instance (Ord a) => Ord (HuffmanTree a) where
    compare (Leaf _ w1) (Leaf _ w2) = compare w1 w2
    compare (Leaf _ w1) (Node _ _ w2) = compare w1 w2
    compare (Node _ _ w1) (Leaf _ w2) = compare w1 w2
    compare (Node _ _ w1) (Node _ _ w2) = compare w1 w2

weight :: HuffmanTree a -> Int
weight (Leaf _ w) = w
weight (Node _ _ w) = w

buildHuffmanTree :: [HuffmanTree a] -> HuffmanTree a
buildHuffmanTree [] = error "Cannot build Huffman tree from empty list"
buildHuffmanTree [t] = t
buildHuffmanTree (t1:t2:ts) = buildHuffmanTree (insertByWeight newNode ts)
  where
    newNode = Node t1 t2 (weight t1 + weight t2)
    insertByWeight node [] = [node]
    insertByWeight node (x:xs)
        | weight node <= weight x = node : x : xs
        | otherwise = x : insertByWeight node xs

generateCodes :: HuffmanTree a -> [(a, String)]
generateCodes tree = generateCodes' tree ""
    where
        generateCodes' (Leaf v _) code = [(v, code)]
        generateCodes' (Node left right _) code =
            generateCodes' left (code ++ "0") ++ generateCodes' right (code ++ "1")

huffmanEncode :: HuffmanTree a -> [(a, String)]
huffmanEncode = generateCodes

huffmanDecode :: [(a, String)] -> String -> [a]
huffmanDecode codes = decodeHelper
    where
        decodeHelper "" = []
        decodeHelper str = let (code, rest) = findCode str codes in code : decodeHelper rest
        findCode _ [] = error "No matching code found"
        findCode s ((symbole, code):xs)
            | code `isPrefixOf` s = (symbole, drop (length code) s)
            | otherwise = findCode s xs
        isPrefixOf prefix str = take (length prefix) str == prefix
