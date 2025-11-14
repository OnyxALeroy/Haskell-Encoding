module Compression where

import Huffman (HuffmanTree(..), huffmanEncode, buildHuffmanTree)
import Data.List (group, sort)

compress :: String -> String
compress input =
    let
        freqList = map (\xs -> (head xs, length xs)) . group . sort $ input
        leaves = [Leaf c f | (c, f) <- freqList]
        tree = buildHuffmanTree leaves
        codes = huffmanEncode tree
        lookupCode c = case lookup c codes of
            Just code -> code
            Nothing -> error "Character not found in Huffman codes"
        encoded = concatMap lookupCode input
        symbolsTable = concatMap (\(c, code) -> [c] ++ ":" ++ code ++ ",") codes
        result = "SYMBOLS: " ++ init symbolsTable ++ "\nDATA: " ++ encoded
    in result

decompress :: String -> String
decompress input = "Decompression not implemented yet"
