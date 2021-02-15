{-# LANGUAGE FlexibleContexts #-}

module Day7 where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.List
import           Data.Maybe

import           Day5 (execUntilOutput)
import           Helpers
import           InputParser

day7Part1 :: IO Int
day7Part1 = do
  raw <- readBy ',' "day7.txt"
  return $ findMaxSignal $ fromList $ read <$> raw

day7Part2 :: IO Int
day7Part2 = do
  raw <- readBy ',' "day7.txt"
  return $ findMaxSignalLooped $ fromList $ read <$> raw

findMaxSignal :: Array Int Int -> Int
findMaxSignal = maximum . (<$> getPerm [0, 1, 2, 3, 4]) . tryCombination

findMaxSignalLooped :: Array Int Int -> Int
findMaxSignalLooped 
  = maximum . (<$> getPerm [5, 6, 7, 8, 9]) . tryCombinationLooped

getPerm :: [Int] -> [[Int]]
getPerm []
  = [[]]
getPerm as
  = [a : permed | a <- as, permed <- getPerm (as \\ [a])]

tryCombination :: Array Int Int -> [Int] -> Int
tryCombination arr phases
  = try phases 0
  where
    try [] n
      = n
    try (p : ps) n
      = try ps $ runST $ do
        arrST <- thawST arr
        snd <$> fromJust <$> execUntilOutput arrST 0 [p, n]

tryCombinationLooped :: Array Int Int -> [Int] -> Int
tryCombinationLooped arr phases 
  = runST $ do
    arrSTST  <- newST1DArrayM $ replicate 5 (thaw arr)
    cursorST <- newST1DArray $ replicate 5 0
    try True 0 0 0 arrSTST cursorST
  where
    try _ 5 _ m arrSTST cursorST
      = try False 0 m m arrSTST cursorST
    try f i n m arrSTST cursorST = do
      arrST <- readArray arrSTST i
      addr  <- readArray cursorST i
      raw   <- execUntilOutput arrST addr $ if f then [phases !! i, m] else [m]
      if isNothing raw
        then return m
        else do
          let Just (c, res) = raw
          writeArray cursorST i c
          try f (i + 1) n res arrSTST cursorST
