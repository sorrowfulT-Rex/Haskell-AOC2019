{-# LANGUAGE FlexibleContexts #-}

module Day9 where 

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Maybe

import           Data.Map

import           Helpers
import           InputParser
day9Part1 :: IO [Integer]
day9Part1 = do
  raw <- readBy ',' "day9.txt"
  return $ execFromStart [1] $ initIntCode $ fromList $ zip [0..] (read <$> raw)

day9Part2 :: IO [Integer]
day9Part2 = do
  raw <- readBy ',' "day9.txt"
  return $ execFromStart [2] $ initIntCode $ fromList $ zip [0..] (read <$> raw)

-- This time changing to using a map instead of mutable array

data OutputSegment = Segment 
  { consumesInput :: Bool
  , curValue :: Maybe Integer
  }

data IntCode = IntCode
  { tape :: Map Integer Integer
  , curIndex :: Integer
  , tempIndex :: Integer
  }

initIntCode :: Map Integer Integer -> IntCode
initIntCode = flip (flip IntCode 0) 0

decode :: Int -> (Int, Int, Int, Int)
decode n
  = (r, r', r'', q'')
  where
    (q, r)     = quotRem n 10
    (q', r')   = quotRem (q `div` 10) 10
    (q'', r'') = quotRem q' 10

execFromStart :: [Integer] -> IntCode -> [Integer]
execFromStart = evalState . execFully

execFully :: [Integer] -> State IntCode [Integer]
execFully inputs = do
  segMaybe <- execOnce (headMaybe inputs)
  if isNothing segMaybe
    then return []
    else do
      let Just (Segment f cV) = segMaybe
      st <- get
      rt <- if f 
        then execFully (tail inputs)
        else execFully inputs
      return $ if isNothing cV
        then rt
        else (fromJust cV) : rt

execOnce :: Maybe Integer -> State IntCode (Maybe OutputSegment)
execOnce input = do
  intCode <- get
  let instr = fromIntegral $ (tape intCode) ! (curIndex intCode)
  if instr == 99
    then return Nothing
    else Just <$> execByOp (decode instr)
  where
    mx 1 x _ _             = x
    mx 0 _ y _             = y
    mx 2 _ y z             = y + z
    execByOp (3, m, _, _)  = do
      IntCode tape cI tI <- get
      let a1   = cI + 1
      let s1   = maybe 0 id $ tape !? a1
      updateTape $ insert (mx m a1 s1 tI) (fromJust input)
      newIndices (cI + 2) tI
      return $ Segment True Nothing
    execByOp (4, m, _, _)  = do
      IntCode tape cI tI <- get
      let a1  = cI + 1
      let s1  = maybe 0 id $ tape !? a1
      let out = maybe 0 id $ tape !? (mx m a1 s1 tI)
      newIndices (cI + 2) tI
      return $ Segment False (Just out)
    execByOp (9, m, _, _)  = do
      IntCode tape cI tI <- get
      let a1  = cI + 1
      let s1  = maybe 0 id $ tape !? a1
      let sft = maybe 0 id $ tape !? (mx m a1 s1 tI)
      newIndices (cI + 2) (tI + sft)
      return $ Segment False Nothing
    execByOp (n, m, m', o) = do
      IntCode tape cI tI <- get
      let a1 = cI + 1
      let a2 = cI + 2
      let a3 = cI + 3
      let s1 = maybe 0 id $ tape !? a1
      let s2 = maybe 0 id $ tape !? a2
      let s3 = maybe 0 id $ tape !? a3
      let m1 = maybe 0 id $ tape !? (mx m a1 s1 tI)
      let m2 = maybe 0 id $ tape !? (mx m' a2 s2 tI)
      let m3 = mx o a3 s3 tI
      case n of
        1 -> do
          updateTape $ insert m3 (m1 + m2)
          newIndices (cI + 4) tI
        2 -> do
          updateTape $ insert m3 (m1 * m2)
          newIndices (cI + 4) tI
        5 -> if m1 == 0
          then newIndices (cI + 3) tI
          else newIndices m2 tI
        6 -> if m1 /= 0
          then newIndices (cI + 3) tI
          else newIndices m2 tI
        7 -> if m1 < m2
          then updateTape (insert m3 1) >> newIndices (cI + 4) tI
          else updateTape (insert m3 0) >> newIndices (cI + 4) tI
        8 -> if m1 == m2
          then updateTape (insert m3 1) >> newIndices (cI + 4) tI
          else updateTape (insert m3 0) >> newIndices (cI + 4) tI
      return $ Segment False Nothing

updateTape :: (Map Integer Integer -> Map Integer Integer) -> State IntCode ()
updateTape f = do
  IntCode tape cI tI <- get
  put $ IntCode (f tape) cI tI

newIndices :: Integer -> Integer -> State IntCode ()
newIndices cI tI = do
  IntCode tape _ _ <- get
  put $ IntCode tape cI tI
