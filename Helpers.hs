{-# LANGUAGE FlexibleContexts #-}

module Helpers where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe


-- Array

type Vec1D e     = Array Int e
type Vec2D e     = Array Int (Vec1D e)
type Mat1D e     = Array (Int, Int) e
type STVec1D s e = STArray s Int e
type STVec2D s e = STArray s Int (STVec1D s e)
type STMat1D s e = STArray s (Int, Int) e

thawST :: (Ix i) => Array i e -> ST s (STArray s i e)
thawST = thaw

freezeST :: (Ix i) => STArray s i e -> ST s (Array i e)
freezeST = freeze

newVec1D :: Foldable f => f a -> Vec1D a
newVec1D xs
  = array (0, length xs - 1) $ zip [0..] (toList xs)

newVec2D :: (Functor f1, Foldable f1, Foldable f2) => f1 (f2 a) -> Vec2D a
newVec2D = newVec1D . fmap newVec1D

newSTVec1D :: Foldable f => f a -> ST s (STVec1D s a)
newSTVec1D = thaw . newVec1D

newSTM :: (Ix i1, Ix i2) 
  => Array i1 (ST s (STArray s i2 a)) 
  -> ST s (STArray s i1 (STArray s i2 a))
newSTM = (>>= thaw) . sequence

newSTVec2D :: (Functor f1, Foldable f1, Foldable f2) 
  => f1 (f2 a) 
  -> ST s (STVec2D s a)
newSTVec2D = newSTM . newVec1D . fmap newSTVec1D

-- Pre: all rows have the same length
newMat1D :: (Functor f1, Foldable f1, Foldable f2) => f1 (f2 a) -> Mat1D a
newMat1D mat
  = array ((0, 0), (length mat - 1, length (head mat') - 1)) zipped
  where
    mat'   = toList mat
    zipped = zip [0..] mat' >>= drag
    drag (i, row)
      = map (\(j, ele) -> ((i, j), ele)) $ zip [0..] (toList row)

newSTMat1D :: (Functor f1, Foldable f1, Foldable f2) 
  => f1 (f2 a) 
  -> ST s (STMat1D s a)
newSTMat1D = thaw . newMat1D

readArrayMaybe :: (MArray a Int m) => a Int Int -> Int -> m (Maybe Int)
readArrayMaybe arrST index = do
  (inf, sup) <- getBounds arrST
  if index > sup || index < inf 
    then return Nothing
    else readArray arrST index >>= return . Just

adjustArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
adjustArray arrST i f = do
  cur <- readArray arrST i
  writeArray arrST i $! f cur


-- Comparison

minimumOn :: (Ord b, Foldable f) => (a -> b) -> f a -> a
minimumOn f
  = minimumBy ((. f) . compare . f)

maximumOn :: (Ord b, Foldable f) => (a -> b) -> f a -> a
maximumOn f
  = maximumBy ((. f) . compare . f)


-- List

headMaybe :: [a] -> Maybe a
headMaybe []
  = Nothing
headMaybe xs
  = Just $ head xs

tailMaybe :: [a] -> Maybe [a]
tailMaybe []
  = Nothing
tailMaybe xs
  = Just $ tail xs


-- Misc.

formatImage :: [String] -> IO ()
formatImage strs
  = forM_ strs printRow
  where
    printRow ""
      = putStr "\n"
    printRow ('0' : cs)
      = putStr " " >> printRow cs
    printRow ('1' : cs)
      = putStr ([chr 9608]) >> printRow cs
