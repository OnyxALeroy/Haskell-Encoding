module Compression where

import Huffman (HuffmanTree(..), huffmanEncode, huffmanDecode, buildHuffmanTree)
import BurrowsWheeler (bwTransform, inverseBWT)
import RunLength (runLengthEncode, runLengthDecode)
import Data.List (group, sort)

compress :: String -> String
compress input =
    let
        (bwtPos, bwtOutput) = bwTransform input
        rleEncoded = runLengthEncode bwtOutput
        rleStr = concatMap (\(count, char) -> show count ++ "$" ++ [char]) rleEncoded
        freqList = map (\xs -> (head xs, length xs)) . group . sort $ rleStr
        leaves = [Leaf c f | (c, f) <- freqList]
        tree = buildHuffmanTree leaves
        codes = huffmanEncode tree
        lookupCode c = case lookup c codes of
            Just code -> code
            Nothing -> error "Character not found in Huffman codes"
        encoded = concatMap lookupCode rleStr
        symbolsTable = concatMap (\(c, code) -> escapeChar c ++ ":" ++ code ++ ",") codes
        result = "FNS: BWT,RLE,HUFFMAN\nSYMBOLS: " ++ init symbolsTable ++ "\nPOSITION: " ++ show bwtPos ++ "\nDATA: " ++ encoded
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
    foldl (\acc fn -> applySingleFunction fn codes position acc) dataStr (reverse functions)

applySingleFunction :: String -> [(Char, String)] -> Int -> String -> String
applySingleFunction "HUFFMAN" codes _ dataStr = huffmanDecode codes dataStr
applySingleFunction "BWT" _ position dataStr = inverseBWT position dataStr
applySingleFunction "RLE" _ _ dataStr =
    if '$' `elem` dataStr
    then runLengthDecode (parseRLEPairs dataStr)
    else dataStr
applySingleFunction _ _ _ _ = error "Unsupported function"

parseRLEPairs :: String -> [(Int, Char)]
parseRLEPairs "" = []
parseRLEPairs str =
    let (numStr, rest) = break (== '$') str
        count = read numStr :: Int
        char = if null (tail rest) then error "Invalid RLE format" else head (tail rest)
        remaining = drop 2 rest
    in (count, char) : parseRLEPairs remaining
