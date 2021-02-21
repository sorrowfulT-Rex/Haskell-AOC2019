module Day13 where

import           Control.Monad.Trans.State
import           Data.Maybe

import           Data.Map hiding (filter)

import           Day9
import           InputParser

day13Part1 :: IO Int
day13Part1 = do
  raw <- readBy ',' "day13.txt"
  let res = playGameRaw $ initIntCode $ fromList $ zip [0..] (read <$> raw)
  return $ length $ filter isBlock $ parseGameResult res

day13Part2 :: IO Integer
day13Part2 = do
  raw <- readBy ',' "day13.txt"
  let cpt       = initIntCode $ fromList $ zip [0..] (2 : tail (read <$> raw))
  let key       = (Nothing, Nothing, Nothing)
  let (_, _, s) = evalState (playWithControl key []) cpt
  return $ fromJust s

playGameRaw :: IntCode -> [Int]
playGameRaw = fmap fromInteger . execFromStart (cycle [10])

data GameElement = GameElement Int Int Int
  deriving (Show)

isBlock :: GameElement -> Bool
isBlock (GameElement _ _ 2)
  = True
isBlock _
  = False

parseGameResult :: [Int] -> [GameElement]
parseGameResult []
  = []
parseGameResult (x : y : t : rest)
  = GameElement x y t : parseGameResult rest

type KeyElements = (Maybe GameElement, Maybe GameElement, Maybe Integer)

playWithControl :: KeyElements -> [Integer] -> State IntCode KeyElements
playWithControl k@(ball, paddle, score) cache = do
  segMaybe <- execOnce (input ball paddle)
  if isNothing segMaybe
    then return k
    else do
      let Just (Segment _ cV) = segMaybe
      if isNothing cV
        then playWithControl k cache
        else do
          let cache' = cache ++ [fromJust cV]
          if length cache' == 3
            then playWithControl (k' cache') []
            else playWithControl k cache'
  where
    input (Just (GameElement bx _ _)) (Just (GameElement px _ _))
      | bx < px   = Just $ -1
      | bx > px   = Just 1
      | otherwise = Just 0
    input _ _
      = Nothing
    k' [h, v, 4]
      = (Just $ GameElement (fromIntegral h) (fromIntegral v) 4, paddle, score)
    k' [h, v, 3]
      = (ball, Just $ GameElement (fromIntegral h) (fromIntegral v) 3, score)
    k' [-1, 0, s]
      = (ball, paddle, Just s)
    k' _
      = k
