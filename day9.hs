{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Day9 where 

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Maybe

import           Data.Map

import           Helpers hiding (fromList)
import           InputParser

import           Debug.Trace

day9Part1 :: IO [Integer]
day9Part1 = do
  raw <- readBy ',' "day9.txt"
  return $ execFromStart [1] $ fromList $ zip [0..] (read <$> raw)

day9Part2 :: IO [Integer]
day9Part2 = do
  raw <- readBy ',' "day9.txt"
  return $ execFromStart [2] $ fromList $ zip [0..] (read <$> raw)

-- This time changing to using a map instead of mutable array

data OutputSegment = Segment 
  { consumesInput :: Bool
  , curIndex :: Integer
  , curValue :: Maybe Integer
  , tempIndex :: Integer
  }

initSeg :: OutputSegment
initSeg = Segment False 0 Nothing 0

decode :: Int -> (Int, Int, Int, Int)
decode n
  = (r, r', r'', q'')
  where
    (q, r)     = quotRem n 10
    (q', r')   = quotRem (q `div` 10) 10
    (q'', r'') = quotRem q' 10

execFromStart :: [Integer] -> Map Integer Integer -> [Integer]
execFromStart = evalState . flip execFully initSeg

execFully :: [Integer] 
  -> OutputSegment 
  -> State (Map Integer Integer) [Integer]
execFully inputs seg = do
  segMaybe <- execOnce (headMaybe inputs) seg
  if isNothing segMaybe
    then return []
    else do
      let Just (Segment f cI cV tI) = segMaybe
      rt <- if f 
        then execFully (tail inputs) (Segment f cI cV tI)
        else execFully inputs (Segment f cI cV tI)
      return $ if isNothing cV
        then rt
        else (fromJust cV) : rt

execOnce :: Maybe Integer 
  -> OutputSegment 
  -> State (Map Integer Integer) (Maybe OutputSegment)
execOnce input seg = do
  intCode <- get
  let instr = fromIntegral $ intCode ! index
  if instr == 99
    then return Nothing
    else Just <$> (execByOp $ decode instr)
  where
    mx 1 x _            = x
    mx 0 _ y            = y
    mx 2 _ y            = y + tempI
    index               = curIndex seg
    tempI               = tempIndex seg
    execByOp (3, m, _, _)  = do
      intCode <- get
      let a1   = index + 1
      let s1   = maybe 0 id $ intCode !? a1
      let addr = maybe 0 id $ intCode !? (mx m a1 s1)
      put $ insert (mx m a1 s1) (fromJust input) intCode
      return $ Segment True (index + 2) Nothing tempI
    execByOp (4, m, _, _)  = do
      intCode <- get
      let a1  = index + 1
      let s1  = maybe 0 id $ intCode !? a1
      let out = maybe 0 id $ intCode !? (mx m a1 s1)
      return $ Segment False (index + 2) (Just out) tempI
    execByOp (9, m, _, _)  = do
      intCode <- get
      let a1  = index + 1
      let s1  = maybe 0 id $ intCode !? a1
      let sft = maybe 0 id $ intCode !? (mx m a1 s1)
      return $ Segment False (index + 2) Nothing (tempI + sft)
    execByOp (n, m, m', o) = do
      intCode <- get
      let a1 = index + 1
      let a2 = index + 2
      let a3 = index + 3
      let s1 = maybe 0 id $ intCode !? a1
      let s2 = maybe 0 id $ intCode !? a2
      let s3 = maybe 0 id $ intCode !? a3
      let m1 = maybe 0 id $ intCode !? (mx m a1 s1)
      let m2 = maybe 0 id $ intCode !? (mx m' a2 s2)
      let m3 = mx o a3 s3
      case n of
        1 -> do
          put $ insert m3 (m1 + m2) intCode
          return $ Segment False (index + 4) Nothing tempI
        2 -> do
          put $ insert m3 (m1 * m2) intCode
          return $ Segment False (index + 4) Nothing tempI
        5 -> return $ if m1 == 0
          then Segment False (index + 3) Nothing tempI
          else Segment False m2 Nothing tempI
        6 -> return $ if m1 /= 0
          then Segment False (index + 3) Nothing tempI
          else Segment False m2 Nothing tempI
        7 -> put (if m1 < m2
          then insert m3 1 intCode
          else insert m3 0 intCode
          ) >> return (Segment False (index + 4) Nothing tempI)
        8 -> put (if m1 == m2
          then insert m3 1 intCode
          else insert m3 0 intCode
          ) >> return (Segment False (index + 4) Nothing tempI)
        _ -> return $ error "Unknown Instruction!"
