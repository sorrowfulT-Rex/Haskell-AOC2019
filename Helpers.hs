{-# LANGUAGE FlexibleContexts #-}

module Helpers where

import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.List
import           Data.Maybe


-- Array

type OneD s e = ST s (STArray s Int e)
type TwoD s e = ST s (STArray s Int (OneD s e))

fromList :: [a] -> Array Int a
fromList xs
  = array (0, length xs - 1) $ zip [0..] xs

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
