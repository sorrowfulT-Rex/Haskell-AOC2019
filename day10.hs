{-# LANGUAGE FlexibleContexts #-}

module Day10 where 

import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.List
import           Data.Maybe

import           Data.Sequence as S (Seq(..), deleteAt, fromList, lookup)

import           Helpers as H
import           InputParser

type AstroidMap = Array Int (Array Int Char)

day10Parser :: [String] -> AstroidMap
day10Parser = H.fromList . fmap H.fromList

day10Part1 :: IO Int
day10Part1 = do
  raw <- readByLines "day10.txt"
  return $ maxSearch $ day10Parser raw

day10Part2 :: IO Int
day10Part2 = do
  raw <- readByLines "day10.txt"
  let astrs  = day10Parser raw
  let spot   = maxSpot astrs
  let (x, y) =find200th spot astrs
  return $ x * 100 + y

height, width :: AstroidMap -> Int
height = length
width  = length . (! 0)

linearSearch :: (Int, Int) -> (Int, Int) -> AstroidMap -> Int
linearSearch (x, y) d@(dx, dy) astrs
  | x' < 0 || x' >= width astrs  = 0
  | y' < 0 || y' >= height astrs = 0
  | astrs ! y' ! x' == '#'       = 1
  | otherwise                    = linearSearch (x', y') d astrs
  where
    x' = x + dx
    y' = y + dy

validRays :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
validRays (xMin, xMax) (yMin, yMax)
  = [(x, y) | x <- [xMin..xMax], y <- [yMin..yMax], gcd x y == 1]

maxSearch :: AstroidMap -> Int
maxSearch astrs
  = maximum $ flip searchCount astrs <$> validSpots astrs

maxSpot :: AstroidMap -> (Int, Int)
maxSpot astrs
  = maximumOn (flip searchCount astrs) (validSpots astrs)

validSpots :: AstroidMap -> [(Int, Int)]
validSpots astrs
  = [(x, y) | x <- xRange, y <- yRange, astrs ! y ! x == '#']
  where
    xRange = [0..(width astrs - 1)]
    yRange = [0..(height astrs - 1)]

searchCount :: (Int, Int) -> AstroidMap -> Int
searchCount (x, y) astrs
  = sum $ flip (linearSearch (x, y)) astrs <$> valids
  where
    valids = validRays (-x, width astrs - x - 1) (-y, height astrs - y - 1)

linearSearchAll :: (Int, Int) -> (Int, Int) -> AstroidMap -> [(Int, Int)]
linearSearchAll (x, y) d@(dx, dy) astrs
  | x' < 0 || x' >= width astrs  = []
  | y' < 0 || y' >= height astrs = []
  | astrs ! y' ! x' == '#'       = (x', y') : rt
  | otherwise                    = rt
  where
    x' = x + dx
    y' = y + dy
    rt = linearSearchAll (x', y') d astrs

orderedValidRays :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
orderedValidRays a b
  = sortOn angle (validRays a b)
  where
    angle (x, y)
      | x >= 0 = acos $ fromIntegral (-y) / sqrt (fromIntegral $ x * x + y * y)
      | x < 0  = pi + angle (-x, -y)

find200th :: (Int, Int) -> AstroidMap -> (Int, Int)
find200th xy@(x, y) astrs
  = runST $ do 
    arrST <- thaw others :: OneD s (Seq (Int, Int))
    find 1 0 arrST
  where
    others 
      = H.fromList $ S.fromList <$> flip (linearSearchAll xy) astrs <$> valids
    valids 
      = orderedValidRays (-x, width astrs - x - 1) (-y, height astrs - y - 1)
    find n i arrST
      | i == length others = find n 0 arrST
    find 200 i arrST = do
      s <- readArray arrST i
      let raw = S.lookup 0 s
      if isNothing raw
        then find 200 (i + 1) arrST
        else return $ fromJust raw
    find n i arrST = do
      s <- readArray arrST i
      let raw = S.lookup 0 s
      if isNothing raw
        then find n (i + 1) arrST
        else do
          writeArray arrST i (deleteAt 0 s)
          find (n + 1) (i + 1) arrST
