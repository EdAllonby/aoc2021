module Five where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import FileUtils (getLines)

data Coordinate = Coordinate {x :: Int, y :: Int} deriving (Show, Eq, Ord)

data Line = Line {start :: Coordinate, end :: Coordinate} deriving (Show, Eq)

solveFirst :: IO ()
solveFirst = do
  puzzle <- getPuzzle
  let lines = parseLine <$> puzzle
  let horizontalOrVeritcalLines = filter isHorizontalOrVerticalLine lines
  print (length . findIntersectingCoordinates $ horizontalOrVeritcalLines)

solveSecond :: IO ()
solveSecond = do
  puzzle <- getPuzzle
  let lines = parseLine <$> puzzle
  print (length . findIntersectingCoordinates $ lines)

getPuzzle :: IO [String]
getPuzzle = getLines "resources/5.txt"

parseLine :: String -> Line
parseLine line = Line start end
  where
    [start, end] = parseCoordinate <$> splitOn "->" line

parseCoordinate :: String -> Coordinate
parseCoordinate coord = let [x, y] = read <$> splitOn "," coord in Coordinate x y

isHorizontalOrVerticalLine :: Line -> Bool
isHorizontalOrVerticalLine (Line (Coordinate x1 y1) (Coordinate x2 y2)) = x1 == x2 || y1 == y2

findIntersectingCoordinates :: [Line] -> [(Int, Coordinate)]
findIntersectingCoordinates lines = filter (\(count, coord) -> count > 1) $ frequency . concat $ findCoordinatesOnLine <$> lines

findCoordinatesOnLine :: Line -> [Coordinate]
findCoordinatesOnLine (Line (Coordinate x1 y1) (Coordinate x2 y2))
  | x1 == x2 = [Coordinate x1 y | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [Coordinate x y1 | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = fmap (uncurry Coordinate) (zip [min x1 x2 .. max x1 x2] [min y1 y2 .. max y1 y2])

isCoordinateOnLine :: Line -> Coordinate -> Bool
isCoordinateOnLine line coord = coord `elem` findCoordinatesOnLine line

frequency :: Ord a => [a] -> [(Int, a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))
