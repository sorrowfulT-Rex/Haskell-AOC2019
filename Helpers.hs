{-# LANGUAGE FlexibleContexts #-}

module Helpers where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Char
import           Data.List
import           Data.Maybe


-- Array

type OneD s e = ST s (STArray s Int e)
type TwoD s e = ST s (STArray s Int (STArray s Int e))

fromList :: [a] -> Array Int a
fromList xs
  = array (0, length xs - 1) $ zip [0..] xs

newST1DArray :: [a] -> OneD s a
newST1DArray = thaw . fromList

newST1DArrayM :: [OneD s a] -> TwoD s a
newST1DArrayM arrSTs = do
  info <- sequence arrSTs
  newST1DArray info

newST2DArray :: [[a]] -> ST s (STArray s Int (STArray s Int a))
newST2DArray = newST1DArrayM . fmap newST1DArray

readArrayMaybe :: (MArray a Int m) => a Int Int -> Int -> m (Maybe Int)
readArrayMaybe arrST index = do
  (inf, sup) <- getBounds arrST
  if index > sup || index < inf 
    then return Nothing
    else readArray arrST index >>= return . Just


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
