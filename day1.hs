module Day1 where

import           Control.Monad.Trans.State

import           InputParser

massOf :: Int -> Int
massOf = (+ (-2)) . (`div` 3)

day1Part1 :: IO Int
day1Part1 = do
  raw <- readByLines "day1.txt"
  return $ sum $ massOf . read <$> raw

massOf' :: Int -> Int
massOf' mod
  = execState (massOfS $ massOf mod) 0
  where
    massOfS m
      | m <= 0    = return ()
      | otherwise = do
        acc <- get
        put $ acc + m
        massOfS $ massOf m

day1Part2 :: IO Int
day1Part2 = do
  raw <- readByLines "day1.txt"
  return $ sum $ massOf' . read <$> raw
