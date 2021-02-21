{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Day12 where 

import           Control.Monad
import           Control.Monad.ST.Strict
import           Data.Array.Unboxed
import           Data.Array.ST
import           Data.Maybe
import           Data.Text hiding (empty, foldl1, length, replicate)
import           Prelude hiding (drop, init, length, tail)

import           Data.Set (Set(..), empty, insert, member, size)

import           Helpers
import           InputParser hiding (split)

day12Part1 :: IO Int
day12Part1 = do
  raw <- readByLines "day12.txt"
  let mat = simulate1000 $ day12Parser raw
  return $ energy mat

day12Part2 :: IO Int
day12Part2 = do
  raw <- readByLines "day12.txt"
  return $ findDejaVuTime $ day12Parser raw

day12Parser :: [String] -> Mat1D Int
day12Parser = newMat1D . fmap day12ParseLine

day12ParseLine :: String -> [Int]
day12ParseLine str
  = pos ++ [0, 0, 0]
  where
    packed = init $ tail $ pack str
    pos    = read . unpack . drop 2 . strip <$> split (== ',') packed

velIndices, posIndices, comparator :: [(Int, Int)]
velIndices = [(i, j) | i <- [0..3], j <- [3..5]]
posIndices = [(i, j) | i <- [0..3], j <- [0..2]]
comparator = [(i, j) | i <- [0..3], j <- [0..3], i < j]

simulate1000 :: Mat1D Int -> Mat1D Int
simulate1000 mat
  = runST $ do
    matST <- thawST mat
    forM_ [1..1000] $ const (simulate matST)
    freezeST matST

emptySTMat :: ST s (STMat1D s Int)
emptySTMat = newSTMat1D $! [replicate 3 0 | _ <- [0..3]]

simulate :: STMat1D s Int -> ST s ()
simulate matST = do
  velsST <- pucci -- 引力を信じるか。
  forM_ velIndices $ \(i, j) -> do 
    newV <- readArray velsST (i, j - 3)
    adjustArray matST (i, j) (+ newV)
  forM_ posIndices $ \(i, j) -> do
    v <- readArray matST (i, j + 3)
    p <- readArray matST (i, j)
    writeArray matST (i, j) $! (p + v)
  where
    pucci = do
      newVelST <- emptySTMat
      forM_ comparator $ pucci' newVelST
      return newVelST
    pucci' velST (i, j) 
      = forM_ [0..2] $ \s -> do
        p1 <- readArray matST (i, s)
        p2 <- readArray matST (j, s)
        adjustArray velST (i, s) (+ (signum (p2 - p1)))
        adjustArray velST (j, s) (+ (signum (p1 - p2)))

energy :: Mat1D Int -> Int
energy mat
  = sum $ energyOfOne <$> [0..3]
  where
    energyOfOne i
      = pE * kE
      where
        pE = sum $ abs <$> [mat ! (i, 0), mat ! (i, 1), mat ! (i, 2)]
        kE = sum $ abs <$> [mat ! (i, 3), mat ! (i, 4), mat ! (i, 5)]

type AxisHashSet = Set[(Int, Int)]

-- Keep the current state separated by axis, then simulate the next state
simulateWithState :: (AxisHashSet, AxisHashSet, AxisHashSet) 
  -> STMat1D s Int 
  -> ST s ((AxisHashSet, AxisHashSet, AxisHashSet))
simulateWithState (x, y, z) matST = do
  x' <- getAxis 0 0
  y' <- getAxis 1 0
  z' <- getAxis 2 0
  simulate matST
  return (insert x' x, insert y' y, insert z' z)
  where
    getAxis i 4 = return []
    getAxis i n = do
      posVel <- readTuple (n, i)
      rest   <- getAxis i (n + 1)
      return $ posVel : rest
    readTuple (s, i)
      = do
        e0 <- readArray matST (s, i)
        e1 <- readArray matST (s, i + 3)
        return (e0, e1)

-- When the three sets for all axis remain the same after a simulation,
-- their sizes are the time required for the corresponding axis to happen again.
-- Since the three axis do not interfere with each other, 
-- their least common multiple sis the answer.
-- This is quite costly; don't be surprised if it goes longer than expected.
findDejaVuTime :: Mat1D Int -> Int
findDejaVuTime mat
  = runST $ do
    matST <- thawST mat
    foo' (empty, empty, empty) 0 0 0 matST
  where
    foo' matStates lX lY lZ matST = do
      matStates' <- simulateWithState matStates matST
      let (x, y, z) = matStates'
      if size x == lX && size y == lY && size z == lZ
        then return $ foldl1 lcm [lX, lY, lZ]
        else foo' matStates' (size x) (size y) (size z) matST
