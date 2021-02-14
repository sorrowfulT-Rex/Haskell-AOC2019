{-# LANGUAGE FlexibleContexts #-}

module Day2 where 

import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST

import           Helpers
import           InputParser

day2Part1 :: IO Int
day2Part1 = do
  raw <- readBy ',' "day2.txt"
  return $ run $ read <$> raw
  where
  run ls = runST $ do
    arrST <- newST1DArray ls
    program1202 arrST
    execute arrST 0
    readArray arrST 0

day2Part2 :: IO Int
day2Part2 = do
  raw <- readBy ',' "day2.txt"
  let (n, v) = run 0 0 $ read <$> raw
  return $ 100 * n + v
  where
    run 100 _ _ = error ""
    run n v ls = runST $ do
      arrST <- newST1DArray ls
      programInit n v arrST
      execute arrST 0
      res   <- readArray arrST 0
      return $ if res == 19690720
        then (n, v)
        else if v == 99
          then run (n + 1) 0 ls
          else run n (v + 1) ls

program1202 :: (MArray a Int m) => a Int Int -> m ()
program1202 = programInit 12 2

programInit :: (MArray a Int m) => Int -> Int -> a Int Int -> m ()
programInit n v arrST = do
  writeArray arrST 1 n
  writeArray arrST 2 v

execute :: (MArray a Int m) => a Int Int -> Int -> m ()
execute arrST i = do
  instr <- readArray arrST i
  if instr == 99
    then return ()
    else do
      a1 <- readArray arrST (i + 1)
      a2 <- readArray arrST (i + 2)
      a3 <- readArray arrST (i + 3)
      s1 <- readArray arrST a1
      s2 <- readArray arrST a2
      writeArray arrST a3 ((getFunc instr) s1 s2)
      execute arrST (i + 4)
  where
    getFunc 1 = (+)
    getFunc 2 = (*)
