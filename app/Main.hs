module Main where

import Compression (compress, decompress, compressReverse)
import BurrowsWheeler (bwTransform, inverseBWT)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["compress", input, output] -> do
            content <- readFile input
            let result = compress content
            writeFile output result
            putStrLn result

        ["decompress", input, output] -> do
            content <- readFile input
            let result = decompress content
            writeFile output result
            putStrLn result

        ["compress-reverse", input, output] -> do
            content <- readFile input
            let result = compressReverse content
            writeFile output result
            putStrLn result

        ["bwt", input] -> do
            let (idx, lastCol) = bwTransform input
            putStrLn (show idx ++ " " ++ lastCol)

        ["rbwt", idxStr, lastCol] -> do
            let idx = read idxStr :: Int
            let result = inverseBWT idx lastCol
            putStrLn result

        _ -> do
            putStrLn "Usage:"
            putStrLn "  haskell-encoding compress <input> <output>"
            putStrLn "  haskell-encoding compress-reverse <input> <output>"
            putStrLn "  haskell-encoding decompress <input> <output>"
            putStrLn "  haskell-encoding bwt <string>"
            putStrLn "  haskell-encoding rbwt <int> <string>"
