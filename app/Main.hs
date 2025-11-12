module Main where

import Compression (compress, decompress)
import System.Environment (getArgs)
import System.IO (hGetContents, stdin)
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["compress", file] -> do
            content <- readFile file
            putStrLn $ compress content
        ["compress"] -> do
            content <- hGetContents stdin
            putStrLn $ compress content
        ["decompress", file] -> do
            content <- readFile file
            putStrLn $ decompress content
        ["decompress"] -> do
            content <- hGetContents stdin
            putStrLn $ decompress content
        _ -> do
            putStrLn "Usage:"
            putStrLn "  haskell-encoding compress [file]"
            putStrLn "  haskell-encoding decompress [file]"
            putStrLn ""
            putStrLn "If no file is provided, reads from stdin."
