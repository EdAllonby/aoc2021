module Three where

import Control.Applicative
import Data.Char (digitToInt)
import Data.List (foldl', tails, transpose)
import FileUtils (getLines, getNumbers)

solveFirst :: IO ()
solveFirst = do
  lines <- getPuzzle
  let rotated = rotr lines
  let res = fmap count rotated
  print rotated
  let gamma = toDec $ fmap mostSignifiant res

  let epsilon = toDec $ fmap leastSignifiant res
  print $ gamma * epsilon

solveSecond :: IO ()
solveSecond = do
  return ()

getPuzzle :: IO [String]
getPuzzle = getLines "resources/3.txt"

rotr :: [[x]] -> [[x]]
rotr = map reverse . transpose

count :: String -> (Int, Int)
count = foldl (\(ones, zeros) a -> if digitToInt a == 0 then (ones, zeros + 1) else (ones + 1, zeros)) (0, 0)

mostSignifiant :: (Int, Int) -> Int
mostSignifiant (ones, zeroes) = if ones >= zeroes then 1 else 0

leastSignifiant :: (Int, Int) -> Int
leastSignifiant (ones, zeroes) = if ones <= zeroes then 1 else 0

toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0