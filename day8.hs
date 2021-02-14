module Day8 where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Char
import           Data.Foldable

import           Helpers
import           InputParser

type Layer = String

day8Part1 :: IO Int
day8Part1 = do
  raw <- readFile "day8.txt"
  let theOne = leastZeros $ layerNumberCount <$> chunk layersize raw
  return $ (theOne ! 1) * (theOne ! 2)

day8Part2 :: IO ()
day8Part2 = do
  raw <- readFile "day8.txt"
  let theOne = mergeLayers $ chunk layersize raw
  formatImage $ chunk width $ toList theOne

width, height, layersize :: Int
width     = 25
height    = 6
layersize = width * height

layerNumberCount :: Layer -> Array Int Int
layerNumberCount layer
  = runST $ do
    arrST <- thaw $ fromList $ replicate 3 0 :: OneD s Int
    forM_ layer $ \ch -> do
      let addr = ord ch - ord '0'
      s1 <- readArray arrST addr
      writeArray arrST addr (s1 + 1)
    freeze arrST

leastZeros :: [Array Int Int] -> Array Int Int
leastZeros arrs
  = minimumOn (! 0) arrs

mergeLayers :: [Layer] -> Array Int Char
mergeLayers layers
  = runST $ do
    arrST <- thaw $ fromList $ replicate layersize '2' :: OneD s Char
    forM_ [0..(layersize - 1)] $ \i -> do
      writeArray arrST i (merged i 0)
    freeze arrST
  where
    layersArr = fromList $ fromList <$> layers
    layerNum  = length layersArr
    merged i n
      | n == layerNum = '2'
      | colour /= '2' = colour
      | otherwise     = merged i (n + 1)
      where
        colour = layersArr ! n ! i
