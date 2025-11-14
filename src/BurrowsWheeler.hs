module BurrowsWheeler where

import Data.List (sort, elemIndex)
import Data.Maybe (fromMaybe)

shiftRight :: [a] -> [a]
shiftRight [] = []
shiftRight xs =
  case reverse xs of
    []     -> []
    (y:ys) -> y : reverse ys

shifts :: [a] -> [[a]]
shifts xs = take (length xs) (iterate shiftRight xs)

sortShifts :: Ord a => [[a]] -> [[a]]
sortShifts = sort

indexInSorted :: (Ord a, Eq a) => [a] -> [[a]] -> Int
indexInSorted xs sorted =
  fromMaybe (-1) (elemIndex xs sorted)

bwTransform :: Ord a => [a] -> (Int, [a])
bwTransform xs =
  let m      = shifts xs
      sorted = sortShifts m
      idx    = indexInSorted xs sorted
      lasts  = map last sorted
  in  (idx + 1, lasts)
