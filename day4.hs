module Day4 where 

import           Control.Applicative
import           Data.List

import           InputParser

intToDigit :: Int -> [Int]
intToDigit = (read <$>) . split ' ' . intersperse ' ' . show

day4Part1 :: IO Int
day4Part1 = do
  (least : [most]) <- split '-' <$> readFile "day4.txt"
  let f = (liftA2 (&&) ascTest adjTest) . intToDigit
  return $ length $ filter f [(read least)..(read most)]

day4Part2 :: IO Int
day4Part2 = do
  (least : [most]) <- split '-' <$> readFile "day4.txt"
  let f = (liftA2 (&&) ascTest adjTestPart2) . intToDigit
  return $ length $ filter f [(read least)..(read most)]

adjTest :: [Int] -> Bool
adjTest (x : x' : xs)
  | x == x'   = True
  | otherwise = adjTest (x' : xs)
adjTest _
  = False

ascTest :: [Int] -> Bool
ascTest (x : x' : xs)
 | x > x'    = False
 | otherwise = ascTest (x' : xs)
ascTest _
  = True

adjTestPart2 :: [Int] -> Bool
adjTestPart2 [x, x']
 | x == x'   = True
 | otherwise = False
adjTestPart2 (x : x' : x'' : xs)
  | x == x'   = (x /= x'') || adjTestPart2 (dropWhile (== x) xs)
  | otherwise = adjTestPart2 (x' : x'' : xs)
adjTestPart2 _
  = False
