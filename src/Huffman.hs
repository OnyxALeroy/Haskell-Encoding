module Huffman where

data HuffmanTree a = Leaf a Int
    | Node (HuffmanTree a) (HuffmanTree a) Int
    deriving (Show, Eq)

instance (Eq a) => Ord (HuffmanTree a) where
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
-- Example usage:
-- let trees = [Leaf 'a' 5, Leaf 'b' 9, Leaf 'c' 12, Leaf 'd' 13, Leaf 'e' 16, Leaf 'f' 45]
-- huffmanEncode trees
-- Output: [('f',"0"),('c',"100"),('d',"101"),('a',"1100"),('b',"1101"),('e',"111")]
