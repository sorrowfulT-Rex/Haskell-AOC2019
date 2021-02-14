{-# LANGUAGE FlexibleContexts #-}

module Day5 where 

import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Maybe

import           Helpers
import           InputParser

day5Part1 :: IO Int
day5Part1 = do
  raw <- readBy ',' "day5.txt"
  return $ run $ read <$> raw
  where
  run ls = runST $ do
    arrST <- newST1DArray ls
    res   <- execWithOneInput arrST 0 1
    return $ last res

day5Part2 :: IO Int
day5Part2 = do
  raw <- readBy ',' "day5.txt"
  return $ run $ read <$> raw
  where
  run ls = runST $ do
    arrST <- newST1DArray ls
    res   <- execWithOneInput arrST 0 5
    return $ last res

execWithOneInput :: (MArray a Int m) => a Int Int -> Int -> Int -> m [Int]
execWithOneInput arrST i input
  = execUntilEnd i
  where
    execUntilEnd i = do
      raw <- execUntilOutput arrST i $ cycle [input]
      if isNothing raw
        then return []
        else do
          let Just (i', n) = raw
          rt <- execUntilEnd i'
          return $ n : rt

execUntilOutput :: (MArray a Int m) 
  => a Int Int 
  -> Int 
  -> [Int] 
  -> m (Maybe (Int, Int))
execUntilOutput arrST i input = do
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
      writeArray arrST a1 (head input)
      execUntilOutput arrST (i + 2) (tail input)
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
        execUntilOutput arrST (i + 4) input
      | op == 5 || op == 6 = do
        a1 <- readArray arrST (i + 1)
        a2 <- readArray arrST (i + 2)
        let (o2, o1) = quotRem (instr `div` 100) 10
        s1 <- readArrayMaybe arrST a1
        s2 <- readArrayMaybe arrST a2
        let (n1, n2) = (mx o1 a1 $ fromJust s1, mx o2 a2 $ fromJust s2)
        if getBool op n1 n1
          then execUntilOutput arrST (i + 3) input
          else execUntilOutput arrST n2 input
      | otherwise          = do
        a1 <- readArray arrST (i + 1)
        a2 <- readArray arrST (i + 2)
        a3 <- readArray arrST (i + 3)
        let (o2, o1) = quotRem (instr `div` 100) 10
        s1 <- readArrayMaybe arrST a1
        s2 <- readArrayMaybe arrST a2
        let (n1, n2) = (mx o1 a1 $ fromJust s1, mx o2 a2 $ fromJust s2)
        writeArray arrST a3 $ if getBool op n1 n2 then 1 else 0
        execUntilOutput arrST (i + 4) input
