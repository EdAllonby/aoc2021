module FileUtils where

getLines :: FilePath -> IO [String]
getLines path = lines <$> readFile path

getNumbers :: FilePath -> IO [Int]
getNumbers path = (fmap . fmap) read (getLines path)