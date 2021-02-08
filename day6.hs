module Day6 where

import           Control.Monad.ST
import           Data.Array.Unboxed as AU

import           Data.Map as M hiding (split)

import           Day2 (fromList)
import           InputParser

-- The idea is to encode each entity into an integer, so that we can use an
-- array to iterate through them quickly.

day6Part1 :: IO Int
day6Part1 = do
  raw <- readByLines "day6.txt"
  let parsed  = day6Parser raw
  let planets = fst <$> parsed
  return $ sumCheck (array (1, sup) parsed) planets
  
day6Part2 :: IO Int
day6Part2 = do
  raw <- readByLines "day6.txt"
  let parsed  = day6Parser raw
  return $ transfer (array (1, sup) parsed) (encode "YOU") (encode "SAN")

literals :: Map Char Int
literals = M.fromList $ zip "1234567890QWERTYUIOPASDFGHJKLZXCVBNM" [0..] 

litNum :: Int
litNum = size literals

end :: Int
end = encode "COM"

sup :: Int
sup = encode "MMM"

you :: Int
you = encode "YOU"

san :: Int
san = encode "SAN"

encode :: String -> Int
encode []
  = 0
encode (l : ls)
  = literals M.! l + litNum * encode ls

supers :: UArray Int Int -> Int -> Int
supers arr i
  | i == end  = 0
  | otherwise = 1 + supers arr (arr AU.! i)

sumCheck :: UArray Int Int -> [Int] -> Int
sumCheck = (sum .) . (<$>) . supers

day6Parser :: [String] -> [(Int, Int)]
day6Parser str = do
  s : [e] <- split ')' <$> str
  return (encode e, encode s)

transfer :: UArray Int Int -> Int -> Int -> Int
transfer arr i j
  | dI <= dJ  = dJ - dI + trans i j'
  | otherwise = dI - dJ + trans i' j
  where
    dI = supers arr i
    dJ = supers arr j
    i' = iter (dI - dJ) (arr AU.!) i
    j' = iter (dJ - dI) (arr AU.!) j
    trans i j
      -- Magic number; not desirable
      -- means I wanna check the parents of i and j intead of themselves
      | i == j    = -2 
      | otherwise = 2 + trans (arr AU.! i) (arr AU.! j)

iter :: Int -> (a -> a) -> a -> a
iter 0 _ x
  = x
iter n f x
  = iter (n - 1) f (f x)
