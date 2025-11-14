module Main where

import Compression (compress, decompress)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["compress", input, output] -> do
            content <- readFile input
            putStrLn $ compress content output
        ["decompress", input, output] -> do
            content <- readFile input
            putStrLn $ decompress content output
        _ -> do
            putStrLn "Usage:"
            putStrLn "  haskell-encoding compress <input> <output>"
            putStrLn "  haskell-encoding decompress <input> <output>"
