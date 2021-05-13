{-# LANGUAGE FlexibleContexts #-}
module Day14 where

import           Control.Monad
import           Control.Monad.Trans.State

import           Data.Map as M hiding (split)

import           Text.Regex.TDFA hiding (empty)

import           InputParser

type Chems = Map String (Integer, Map String Integer)

data SearchStatus = SS
  { need  :: Map String Integer
  , stock :: Map String Integer
  , ores  :: Integer
  }
  deriving (Show)

initSS :: SearchStatus
initSS = SS {need = fromList [("FUEL", 1)], stock = empty, ores = 0}

day14Parser :: [String] -> Chems
day14Parser
  = Prelude.foldl go empty
  where
    go chems str = insert oN (read oC :: Integer, inDict) chems
      where
        materials = getAllTextMatches (str =~ "[0-9]+ [A-Z]+") :: [String]
        outs      = last materials
        ins       = init materials
        [oC, oN]  = words outs
        inSplit   = words <$> ins
        inC       = read . head <$> inSplit :: [Integer]
        inN       = (!! 1) <$> inSplit
        inDict    = fromList $ zip inN inC

computeOres :: Chems -> State SearchStatus Integer
computeOres chems 
  = go >> ores <$> get
  where
    go = do
      SS needs stocks ore <- get
      unless (M.null needs) $ do
        let element = head $ keys needs
        let reqNum  = needs ! element
        let curNum  = findWithDefault 0 element stocks
        let needs'  = delete element needs
        put $ SS needs' stocks ore
        SS needs stocks ore <- get
        if reqNum <= curNum
          then do
            let stocks' = adjust (+ (-reqNum)) element stocks
            put $ SS needs' stocks' ore
          else do
            let (stdNum, mats) = chems ! element
            let multiplier     = if (reqNum - curNum) `mod` stdNum == 0
                  then (reqNum - curNum) `div` stdNum
                  else 1 + (reqNum - curNum) `div` stdNum
            forM_ (assocs mats) $ \(n, c) -> do
              SS ns ss os <- get
              if n == "ORE"
                then put $ SS ns ss (os + c * multiplier)
                else do
                  let ns' = if n `member` ns
                        then adjust (+ c * multiplier) n ns
                        else insert n (c * multiplier) ns
                  put $ SS ns' ss os
            ss <- get
            let needs    = need ss
            let stocks   = stock ss
            let stocks'  = adjust (const 0) element stocks
            let stocks'' = if element `member` stocks'
                  then adjust (+ (multiplier * stdNum - reqNum + curNum))
                    element stocks'
                  else insert element (multiplier * stdNum - reqNum + curNum)
                    stocks'
            put SS {need = needs, stock = stocks'', ores = ores ss}
        ss <- get
        go

day14Part1 :: IO ()
day14Part1 = do
  raw <- readByLines "day14.txt"
  print $ evalState (computeOres $ day14Parser raw) initSS

day14Part2 :: IO ()
day14Part2 = do
  raw <- readByLines "day14.txt"
  let tril  = round 1e12
  let chems = computeOres $ day14Parser raw
  let inf   = tril `div` evalState chems initSS
  let sup   = 10 * inf
  let go inf sup
        | inf + 1 >= sup = inf
        | evl < tril     = go mid sup
        | evl > tril     = go inf mid
        | otherwise      = inf
        where
          evl = evalState chems initSS {need = fromList [("FUEL", mid)]}
          mid = (inf + sup) `div` 2
  print $ go inf sup
