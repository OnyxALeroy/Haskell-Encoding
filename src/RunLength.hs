module RunLength where

import Data.List (group)

runLengthEncode :: Eq a => [a] -> [(Int, a)]
runLengthEncode [] = []
runLengthEncode xs = map (\g -> (length g, head g)) (group xs)

runLengthDecode :: [(Int, a)] -> [a]
runLengthDecode [] = []
runLengthDecode pairs = concatMap (\(count, value) -> replicate count value) pairs