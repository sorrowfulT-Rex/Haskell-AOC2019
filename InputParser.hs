module InputParser where

readByLines :: FilePath -> IO [String]
readByLines = fmap lines . readFile
