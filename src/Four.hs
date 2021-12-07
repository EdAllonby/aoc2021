module Four where

import Data.List (transpose, (\\))
import Data.List.Split
import Debug.Trace (trace, traceShow)
import FileUtils (getLines)

type Grid = [[Int]]

solveFirst :: IO ()
solveFirst = do
  commands <- getPuzzle
  let bingo = fmap (\n -> read n :: Int) $ splitOn "," $ head commands
  let grids = fmap stringToInts <$> chunksOf 5 (filter (/= "") $ tail commands)
  let r = run' grids bingo
  let (dealtBingo, matchedGrid) = head r
  let unmarkedBingo = nonMatching dealtBingo (last matchedGrid)
  let answer = sum unmarkedBingo * last dealtBingo
  print answer

solveSecond :: IO ()
solveSecond = do
  commands <- getPuzzle
  let bingo = fmap (\n -> read n :: Int) $ splitOn "," $ head commands
  let grids = fmap stringToInts <$> chunksOf 5 (filter (/= "") $ tail commands)
  let r = run' grids bingo
  let (dealtBingo, matchedGrid) = last r
  let unmarkedBingo = nonMatching dealtBingo (last matchedGrid)
  let answer = sum unmarkedBingo * last dealtBingo
  print answer

getPuzzle :: IO [String]
getPuzzle = getLines "resources/4.txt"

stringToInts :: String -> [Int]
stringToInts = (read <$>) . words

nonMatching :: [Int] -> Grid -> [Int]
nonMatching bingo grid = filter (`notElem` bingo) (concat grid)

matching :: [Int] -> Grid -> [Int]
matching bingo grid = filter (\item -> item `elem` concat grid) bingo

run' :: [Grid] -> [Int] -> [([Int], [Grid])]
run' grids = tail . snd . foldl (getAllBingoWinners grids) ([], [([], [[]])])

getAllBingoWinners :: [Grid] -> ([Int], [([Int], [Grid])]) -> Int -> ([Int], [([Int], [Grid])])
getAllBingoWinners grids (bingoNumbers, grid) n = if hasChange then (newBingoNumbers, grid ++ [(newBingoNumbers, difference)]) else (newBingoNumbers, grid)
  where
    newBingoNumbers = bingoNumbers ++ [n]
    lastWinningGrids = findWinningGrids bingoNumbers grids
    winningGrids = findWinningGrids newBingoNumbers grids
    hasChange = length winningGrids > length lastWinningGrids
    difference = winningGrids \\ lastWinningGrids

findWinningGrids :: [Int] -> [Grid] -> [Grid]
findWinningGrids = filter . isBingo

isBingo :: [Int] -> Grid -> Bool
isBingo numbers grid = any (isMatch numbers) grid || any (isMatch numbers) (transpose grid)

isMatch :: [Int] -> [Int] -> Bool
isMatch = all . flip elem