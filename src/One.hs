module One where

import Control.Applicative
import Data.List (tails)
import FileUtils (getNumbers)

solveFirst :: IO ()
solveFirst = do
  numbers <- getPuzzle
  let (_, result) = findIncreases numbers
  print result

solveSecond :: IO ()
solveSecond = do
  numbers <- getPuzzle
  print $ findIncreases $ map sum (windows 3 numbers)

getPuzzle :: IO [Int]
getPuzzle = getNumbers "resources/1.txt"

findIncreases :: (Ord a1, Num a2) => [a1] -> (a1, a2)
findIncreases n = foldl (\(b, c) a -> if a > b then (a, c + 1) else (a, c)) (head n, 0) n

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails
