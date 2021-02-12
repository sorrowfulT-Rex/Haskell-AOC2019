{-# LANGUAGE FlexibleContexts #-}

module Day7 where

import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.List ((\\))
import           Data.Maybe

import           Day2 (fromList)
import           Day5 (readArrayMaybe)
import           InputParser

day7Part1 :: IO Int
day7Part1 = do
  raw <- readBy ',' "day7.txt"
  return $ findMaxSignal $ fromList $ read <$> raw

day7Part2 :: IO Int
day7Part2 = do
  raw <- readBy ',' "day7.txt"
  return $ findMaxSignal' $ fromList $ read <$> raw

findMaxSignal :: Array Int Int -> Int
findMaxSignal = maximum . (<$> getPerm [0, 1, 2, 3, 4]) . tryCombination

findMaxSignal' :: Array Int Int -> Int
findMaxSignal' = maximum . (<$> getPerm [5, 6, 7, 8, 9]) . tryCombination'

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
      = try ps (runST $ do
        arrST <- thaw arr :: ST s (STArray s Int Int)
        snd <$> fromJust <$> executeOnce arrST 0 [p, n]
        )

type TwoD s = ST s (STArray s Int (Array Int Int))

tryCombination' :: Array Int Int -> [Int] -> Int
tryCombination' arr phases 
  = runST $ do
    arrSTST  <- thaw $ fromList (replicate 5 arr) :: TwoD s
    cursorST <- thaw $ fromList (replicate 5 0) :: ST s (STArray s Int Int)
    try True 0 0 0 arrSTST cursorST
  where
    try _ 5 _ m arrSTST cursorST
      =  try False 0 m m arrSTST cursorST
    try f i n m arrSTST cursorST = do
      arr   <- readArray arrSTST i
      addr  <- readArray cursorST i
      arrST <- thaw arr :: ST s (STArray s Int Int)
      raw   <- executeOnce arrST addr $ if f then [phases !! i, m] else [m]
      if isNothing raw
        then return m
        else do
          let Just (c, res) = raw
          newArr <- freeze arrST
          writeArray arrSTST i newArr
          writeArray cursorST i c
          try f (i + 1) n res arrSTST cursorST

executeOnce :: (MArray a Int m) 
  => a Int Int 
  -> Int 
  -> [Int] 
  -> m (Maybe (Int, Int))
executeOnce arrST i input = do
  instr <- readArray arrST i
  let opcode = instr `mod` 10
  if instr == 99
    then return Nothing
    else execByOp opcode instr
  where
    getFunc 1        = (+)
    getFunc 2        = (*)
    getBool 5        = const (== 0)
    getBool 6        = const (/= 0)
    getBool 7        = (<)
    getBool 8        = (==)
    mx 1 x _         = x
    mx 0 _ y         = y
    execByOp 3 _     = do
      a1 <- readArray arrST (i + 1)
      if null input
        then return $ error ""
        else do
          let (h : t) = input
          writeArray arrST a1 h
          executeOnce arrST (i + 2) t
    execByOp 4 instr = do
      a1 <- readArray arrST (i + 1)
      s1 <- readArrayMaybe arrST a1
      return $ Just (i + 2, if instr - 4 == 0 then fromJust s1 else a1)
    execByOp op instr
      | op == 1 || op == 2 = do
        a1 <- readArray arrST (i + 1)
        a2 <- readArray arrST (i + 2)
        a3 <- readArray arrST (i + 3)
        let (o2, o1) = quotRem (instr `div` 100) 10
        s1 <- readArrayMaybe arrST a1
        s2 <- readArrayMaybe arrST a2
        let (n1, n2) = (mx o1 a1 $ fromJust s1, mx o2 a2 $ fromJust s2)
        writeArray arrST a3 $ getFunc op n1 n2
        executeOnce arrST (i + 4) input
      | op == 5 || op == 6 = do
        a1 <- readArray arrST (i + 1)
        a2 <- readArray arrST (i + 2)
        let (o2, o1) = quotRem (instr `div` 100) 10
        s1 <- readArrayMaybe arrST a1
        s2 <- readArrayMaybe arrST a2
        let (n1, n2) = (mx o1 a1 $ fromJust s1, mx o2 a2 $ fromJust s2)
        if getBool op n1 n1
          then executeOnce arrST (i + 3) input
          else executeOnce arrST n2 input
      | otherwise          = do
        a1 <- readArray arrST (i + 1)
        a2 <- readArray arrST (i + 2)
        a3 <- readArray arrST (i + 3)
        let (o2, o1) = quotRem (instr `div` 100) 10
        s1 <- readArrayMaybe arrST a1
        s2 <- readArrayMaybe arrST a2
        let (n1, n2) = (mx o1 a1 $ fromJust s1, mx o2 a2 $ fromJust s2)
        writeArray arrST a3 $ if getBool op n1 n2 then 1 else 0
        executeOnce arrST (i + 4) input
