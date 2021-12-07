module Two where

import Data.List (isPrefixOf, tails)
import FileUtils (getLines)

solveFirst :: IO ()
solveFirst = do
  commands <- getPuzzle
  let (horizontal, depth) = foldl getFinalCoordinates (0, 0) commands
  print $ horizontal * depth

solveSecond :: IO ()
solveSecond = do
  commands <- getPuzzle
  let (horizontal, depth, _) =
        foldl getFinalCoordinatesWithAim (0, 0, 0) commands
  print $ horizontal * depth

getFinalCoordinates :: (Int, Int) -> String -> (Int, Int)
getFinalCoordinates (horizontal, depth) command
  | "forward" `isPrefixOf` command = (horizontal + commandAmount, depth)
  | "up" `isPrefixOf` command = (horizontal, depth - commandAmount)
  | "down" `isPrefixOf` command = (horizontal, depth + commandAmount)
  | otherwise = error "not exhaustive"
  where
    commandAmount = getCommandAmount command

getFinalCoordinatesWithAim :: (Int, Int, Int) -> String -> (Int, Int, Int)
getFinalCoordinatesWithAim (horizontal, depth, aim) command
  | "forward" `isPrefixOf` command =
    (horizontal + commandAmount, depth + (aim * commandAmount), aim)
  | "up" `isPrefixOf` command = (horizontal, depth, aim - commandAmount)
  | "down" `isPrefixOf` command = (horizontal, depth, aim + commandAmount)
  | otherwise = error "not exhaustive"
  where
    commandAmount = getCommandAmount command

getCommandAmount :: String -> Int
getCommandAmount command = read (last $ words command) :: Int

getPuzzle :: IO [String]
getPuzzle = getLines "resources/2.txt"
