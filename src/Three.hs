module Three where

import Data.Char (digitToInt, intToDigit)
import Data.List (foldl', tails, transpose)
import FileUtils (getLines, getNumbers)

solveFirst :: IO ()
solveFirst = do
  lines <- getPuzzle
  let rotated = transpose lines
  let res = fmap count rotated
  let gamma = toDec $ fmap mostSignificant res
  let epsilon = toDec $ fmap leastSignificant res
  print $ gamma * epsilon

solveSecond :: IO ()
solveSecond = do
  lines <- getPuzzle
  let oxygen = head $ untilWithCount ((== 1) . length) (`filterForPosition` mostSignificant) lines
  let scrubber = head $ untilWithCount ((== 1) . length) (`filterForPosition` leastSignificant) lines
  print $ binaryStringToInt oxygen * binaryStringToInt scrubber

getPuzzle :: IO [String]
getPuzzle = getLines "resources/3.txt"

untilWithCount :: ([String] -> Bool) -> (Int -> [String] -> [String]) -> [String] -> [String]
untilWithCount p f x = run 0 p f x
  where
    run n p f x
      | p x = x
      | otherwise = run (n + 1) p f (f n x)

filterForPosition :: Int -> ((Int, Int) -> Int) -> [String] -> [String]
filterForPosition n f lines = filter (\l -> digitToInt (l !! n) == mostCommon) lines
  where
    line = transpose lines !! n
    mostCommon = f $ count line

count :: String -> (Int, Int)
count = foldl (\(ones, zeros) a -> if digitToInt a == 0 then (ones, zeros + 1) else (ones + 1, zeros)) (0, 0)

mostSignificant :: (Int, Int) -> Int
mostSignificant (ones, zeroes) = if ones >= zeroes then 1 else 0

leastSignificant :: (Int, Int) -> Int
leastSignificant (ones, zeroes) = if ones < zeroes then 1 else 0

toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0

binaryStringToInt :: String -> Int
binaryStringToInt = toDec . fmap digitToInt