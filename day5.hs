{-# LANGUAGE FlexibleContexts #-}

module Day5 where 

import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Maybe

import           Day2 (fromList)
import           InputParser

day5Part1 :: IO Int
day5Part1 = do
  raw <- readBy ',' "day5.txt"
  return $ run $ fromList $ read <$> raw
  where
  run arr = runST $ do
    arrST <- thaw arr :: ST s (STArray s Int Int)
    res   <- execute arrST 0
    return $ last res

day5Part2 :: IO Int
day5Part2 = do
  raw <- readBy ',' "day5.txt"
  return $ run $ fromList $ read <$> raw
  where
  run arr = runST $ do
    arrST <- thaw arr :: ST s (STArray s Int Int)
    res   <- execute' arrST 0
    return $ last res

readArrayMaybe :: (MArray a Int m) => a Int Int -> Int -> m (Maybe Int)
readArrayMaybe arrST index = do
  (inf, sup) <- getBounds arrST
  if index > sup || index < inf 
    then return Nothing
    else readArray arrST index >>= return . Just    

execute :: (MArray a Int m) => a Int Int -> Int -> m [Int]
execute arrST i = do
  instr <- readArray arrST i
  let opcode = instr `mod` 10
  if instr == 99
    then return []
    else case opcode of
      3 -> do
        a1 <- readArray arrST (i + 1)
        writeArray arrST a1 1
        execute arrST (i + 2)
      4 -> do
        a1 <- readArray arrST (i + 1)
        s1 <- readArrayMaybe arrST a1
        rt <- execute arrST (i + 2)
        return $ (if instr - 4 == 0 then fromJust s1 else a1) : rt
      _ -> do
        a1 <- readArray arrST (i + 1)
        a2 <- readArray arrST (i + 2)
        a3 <- readArray arrST (i + 3)
        let (o2, o1) = quotRem (instr `div` 100) 10
        s1 <- readArrayMaybe arrST a1
        s2 <- readArrayMaybe arrST a2
        let (n1, n2) = (mx o1 a1 $ fromJust s1, mx o2 a2 $ fromJust s2)
        writeArray arrST a3 $ getFunc opcode n1 n2
        execute arrST (i + 4)
  where
    getFunc 1 = (+)
    getFunc 2 = (*)
    mx 1 x _  = x
    mx 0 _ y  = y

execute' :: (MArray a Int m) => a Int Int -> Int -> m [Int]
execute' arrST i = do
  instr <- readArray arrST i
  let opcode = instr `mod` 10
  if instr == 99
    then return []
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
      writeArray arrST a1 5
      execute' arrST (i + 2)
    execByOp 4 instr = do
      a1 <- readArray arrST (i + 1)
      s1 <- readArrayMaybe arrST a1
      rt <- execute' arrST (i + 2)
      return $ (if instr - 4 == 0 then fromJust s1 else a1) : rt
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
        execute' arrST (i + 4)
      | op == 5 || op == 6 = do
        a1 <- readArray arrST (i + 1)
        a2 <- readArray arrST (i + 2)
        let (o2, o1) = quotRem (instr `div` 100) 10
        s1 <- readArrayMaybe arrST a1
        s2 <- readArrayMaybe arrST a2
        let (n1, n2) = (mx o1 a1 $ fromJust s1, mx o2 a2 $ fromJust s2)
        if getBool op n1 n1
          then execute' arrST (i + 3)
          else execute' arrST n2
      | otherwise          = do
        a1 <- readArray arrST (i + 1)
        a2 <- readArray arrST (i + 2)
        a3 <- readArray arrST (i + 3)
        let (o2, o1) = quotRem (instr `div` 100) 10
        s1 <- readArrayMaybe arrST a1
        s2 <- readArrayMaybe arrST a2
        let (n1, n2) = (mx o1 a1 $ fromJust s1, mx o2 a2 $ fromJust s2)
        writeArray arrST a3 $ if getBool op n1 n2 then 1 else 0
        execute' arrST (i + 4)
