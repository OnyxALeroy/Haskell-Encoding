module BurrowsWheeler where

import Data.List (sort)

main :: IO ()
main = do
  let s = shifts "ABEACADABEA"
  putStrLn "Original: ABEACADABEA"
  putStrLn "Unsorted:"
  mapM_ print s
  putStrLn "\nSorted:"
  mapM_ print (sortShifts s)

shiftRight :: [a] -> [a]
shiftRight [] = []
shiftRight xs =
  case reverse xs of
    []     -> []
    (y:ys) -> y : reverse ys

shifts :: [a] -> [[a]]
shifts xs = take (length xs) (iterate shiftRight xs)

sortShifts :: Ord a => [[a]] -> [[a]]
sortShifts xss = sort xss

indexInSorted :: (Ord a, Eq a) => [a] -> [[a]] -> Int
indexInSorted xs sorted =
  fromMaybe (-1) (elemIndex xs sorted)