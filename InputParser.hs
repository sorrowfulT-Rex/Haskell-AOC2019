module InputParser where

readByLines :: FilePath -> IO [String]
readByLines = fmap lines . readFile

readBy :: Char -> FilePath -> IO [String]
readBy = (. readFile) . fmap . InputParser.split

split :: Eq a => a -> [a] -> [[a]]
split x xs 
  | null xs'' = [xs']
  | otherwise = xs' : split x (tail xs'')
  where
    (xs', xs'') = break (== x) xs

chunk :: Int -> [a] -> [[a]]
chunk n xs
  = chunk' xs
  where
    chunk' []
      = []
    chunk' xs
      = x : chunk' xs'
      where
        (x, xs') = splitAt n xs

trimPre :: String -> String
trimPre (' ' : s)
  = s
trimPre s
  = s
