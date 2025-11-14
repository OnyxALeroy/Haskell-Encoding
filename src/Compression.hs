module Compression where

import Huffman (HuffmanTree(..), huffmanEncode, huffmanDecode, buildHuffmanTree)
import BurrowsWheeler (bwTransform, inverseBWT)
import Data.List (group, sort)

compress :: String -> String
compress input =
    let
        (bwtPos, bwtOutput) = bwTransform input
        freqList = map (\xs -> (head xs, length xs)) . group . sort $ bwtOutput
        leaves = [Leaf c f | (c, f) <- freqList]
        tree = buildHuffmanTree leaves
        codes = huffmanEncode tree
        lookupCode c = case lookup c codes of
            Just code -> code
            Nothing -> error "Character not found in Huffman codes"
        encoded = concatMap lookupCode bwtOutput
        symbolsTable = concatMap (\(c, code) -> escapeChar c ++ ":" ++ code ++ ",") codes
        result = "FNS: BWT,HUFFMAN\nSYMBOLS: " ++ init symbolsTable ++ "\nPOSITION: " ++ show bwtPos ++ "\nDATA: " ++ encoded
    in result

decompress :: String -> String
decompress input =
    let
        linesList = lines input
        fnsLine = findLine "FNS:" linesList
        symbolsLine = findLine "SYMBOLS:" linesList
        positionLine = findLine "POSITION:" linesList
        dataLine = findLine "DATA:" linesList

        functions = parseFns fnsLine
        symbolsStr = drop (length "SYMBOLS: ") symbolsLine
        positionStr = drop (length "POSITION: ") positionLine
        dataStr = drop (length "DATA: ") dataLine

        codes = parseSymbols symbolsStr
        position = read positionStr :: Int

        decoded = applyDecompression functions codes position dataStr
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

findLine :: String -> [String] -> String
findLine prefix linesList = head [line | line <- linesList, prefix `isPrefixOf` line]
    where
        isPrefixOf pref str = take (length pref) str == pref

parseFns :: String -> [String]
parseFns fnsLine =
    let fnsStr = drop (length "FNS: ") fnsLine
    in splitBy ',' fnsStr

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy delimiter str =
    let (part, rest) = break (== delimiter) str
    in part : if null rest then [] else splitBy delimiter (tail rest)

applyDecompression :: [String] -> [(Char, String)] -> Int -> String -> String
applyDecompression functions codes position dataStr =
    let huffmanDecoded = huffmanDecode codes dataStr
    in case functions of
        ["BWT", "HUFFMAN"] -> inverseBWT position huffmanDecoded
        ["HUFFMAN", "BWT"] -> inverseBWT position huffmanDecoded  -- Apply inverse BWT after Huffman
        _ -> error "Unsupported function order"
