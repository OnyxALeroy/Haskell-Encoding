module BurrowsWheeler where

import Data.List (sort, elemIndex)
import Data.Maybe (fromMaybe)

-- Encoding algorithm

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

-- Decoding algorithm

emptyTable :: Int -> [[a]]
emptyTable n = replicate n []

prependColumn :: [a] -> [[a]] -> [[a]]
prependColumn col table = zipWith (:) col table

step :: Ord a => [a] -> [[a]] -> [[a]]
step lastCol table =
    let newTable = prependColumn lastCol table
    in  sort newTable

reconstructTable :: Ord a => [a] -> [[a]]
reconstructTable lastCol =
    let n = length lastCol
        initial = emptyTable n
    in  iterate (step lastCol) initial !! n

inverseBWT :: Ord a => Int -> [a] -> [a]
inverseBWT idx lastCol =
    let table = reconstructTable lastCol
    in  table !! (idx - 1)
