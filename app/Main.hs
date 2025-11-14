module Main where

import Compression (compress, decompress)
import BurrowsWheeler (bwTransform)
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
                    
        ["bwt", input] -> do
            let (idx, lastCol) = bwTransform input
            putStrLn (show idx ++ " " ++ lastCol)

        _ -> do
            putStrLn "Usage:"
            putStrLn "  haskell-encoding compress <input> <output>"
            putStrLn "  haskell-encoding decompress <input> <output>"
            putStrLn "  haskell-encoding bwt <string>"
