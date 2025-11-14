module Main where

import Compression (compress, decompress)
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
        _ -> do
            putStrLn "Usage:"
            putStrLn "  haskell-encoding compress <input> <output>"
            putStrLn "  haskell-encoding decompress <input> <output>"
