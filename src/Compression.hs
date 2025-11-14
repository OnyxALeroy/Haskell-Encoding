module Compression where

import Huffman (HuffmanTree(..), huffmanEncode, huffmanDecode, buildHuffmanTree)
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
        symbolsTable = concatMap (\(c, code) -> escapeChar c ++ ":" ++ code ++ ",") codes
        result = "SYMBOLS: " ++ init symbolsTable ++ "\nDATA: " ++ encoded
    in result

decompress :: String -> String
decompress input =
    let
        symbolsLine = lines input !! 0
        symbolsStr = drop (length "SYMBOLS: ") symbolsLine
        dataLine = lines input !! 1
        dataStr = drop (length "DATA: ") dataLine
        codes = parseSymbols symbolsStr
        decoded = huffmanDecode codes dataStr
    in decoded

parseSymbols :: String -> [(Char, String)]
parseSymbols "" = []
parseSymbols str =
    let (symbolPair, rest) = parseSymbolPair str
    in symbolPair : parseSymbols (drop 1 rest)

parseSymbolPair :: String -> ((Char, String), String)
parseSymbolPair str =
    let (escapedChar, afterColon) = break (== ':') str
        char = unescapeChar escapedChar
        (code, afterComma) = break (== ',') (tail afterColon)
    in ((char, code), afterComma)

unescapeChar :: String -> Char
unescapeChar "\\n" = '\n'
unescapeChar "\\r" = '\r'
unescapeChar "\\t" = '\t'
unescapeChar "\\\\" = '\\'
unescapeChar [c] = c
unescapeChar _ = error "Invalid escape sequence"

escapeChar :: Char -> String
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar '\\' = "\\\\"
escapeChar c = [c]
