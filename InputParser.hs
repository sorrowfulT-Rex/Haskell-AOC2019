module InputParser where

readByLines :: FilePath -> IO [String]
readByLines = fmap lines . readFile

readBy :: Char -> FilePath -> IO [String]
readBy = (. readFile) . fmap . split

split :: Eq a => a -> [a] -> [[a]]
split x xs 
  | null xs'' = [xs']
  | otherwise = xs' : split x (tail xs'')
  where
    (xs', xs'') = break (== x) xs
