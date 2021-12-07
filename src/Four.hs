{-# LANGUAGE OverloadedStrings #-}

module Four where

import Data.List (transpose)
import Data.List.Split
import FileUtils (getLines)

solveFirst :: IO ()
solveFirst = do
  commands <- getPuzzle
  let bingo = fmap (\n -> read n :: Int) $ splitOn "," $ head commands
  let grids = fmap stringToInts <$> chunksOf 5 (filter (/= "") $ tail commands)
  print $ length grids

solveSecond :: IO ()
solveSecond = do
  commands <- getPuzzle
  print "2"

getPuzzle :: IO [String]
getPuzzle = getLines "resources/4.txt"

stringToInts :: String -> [Int]
stringToInts grid = (\word -> read word :: Int) <$> words grid

isBingo :: [Int] -> [[Int]] -> Bool
isBingo numbers grid = any (isMatch numbers) grid || any (isMatch numbers) (transpose grid)

isMatch :: [Int] -> [Int] -> Bool
isMatch numbers = all (`elem` numbers)