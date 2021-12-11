import FiveSpec (runTests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $
  do
    runTests