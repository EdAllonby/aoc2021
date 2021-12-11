module FiveSpec where

import Five
import Test.Hspec

runTests = describe "FiveTests" $ do
  it "should return the correct coordinates on vertical line" $ do
    findCoordinatesOnLine (Line (Coordinate 1 3) (Coordinate 1 1)) `shouldMatchList` [Coordinate {x = 1, y = 1}, Coordinate {x = 1, y = 2}, Coordinate {x = 1, y = 3}]

  it "should return the correct coordinates on horizontal line" $ do
    findCoordinatesOnLine (Line (Coordinate 1 1) (Coordinate 3 1)) `shouldMatchList` [Coordinate {x = 1, y = 1}, Coordinate {x = 2, y = 1}, Coordinate {x = 3, y = 1}]

  it "should return the correct coordinates on an upwards right diagonal line" $ do
    findCoordinatesOnLine (Line (Coordinate 1 1) (Coordinate 3 3)) `shouldMatchList` [Coordinate {x = 1, y = 1}, Coordinate {x = 2, y = 2}, Coordinate {x = 3, y = 3}]

  it "should return the correct coordinates on an downwards left diagonal line" $ do
    findCoordinatesOnLine (Line (Coordinate 3 3) (Coordinate 1 1)) `shouldMatchList` [Coordinate {x = 1, y = 1}, Coordinate {x = 2, y = 2}, Coordinate {x = 3, y = 3}]

  it "should return the correct coordinates on another downwards left diagonal line" $ do
    findCoordinatesOnLine (Line (Coordinate 9 7) (Coordinate 7 9)) `shouldMatchList` [Coordinate {x = 9, y = 7}, Coordinate {x = 8, y = 8}, Coordinate {x = 7, y = 9}]

  it "should return a Zero slope on a vertical line" $ do
    slope (Line (Coordinate 1 3) (Coordinate 1 1)) `shouldBe` Zero

  it "should return a Zero slope on a horizontal line" $ do
    slope (Line (Coordinate 1 1) (Coordinate 3 1)) `shouldBe` Zero

  it "should return a Positive slope on an upwards right diagonal line" $ do
    slope (Line (Coordinate 3 3) (Coordinate 1 1)) `shouldBe` Positive

  it "should return a Negative slope on a downwards left diagonal line" $ do
    slope (Line (Coordinate 9 7) (Coordinate 7 9)) `shouldBe` Negative
