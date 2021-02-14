module Day11 where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Maybe

import           Data.Map

import           Day9
import           Helpers hiding (fromList)
import           InputParser

type Panel = (Integer, Integer)
type RobotPos = ((Integer, Integer), Direction)
type RobotPainting = Map (Integer, Integer) Integer

data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Eq, Ord)


turnLeft, turnRight :: Direction -> Direction
turnLeft UP     = LEFT
turnLeft LEFT   = DOWN
turnLeft DOWN   = RIGHT
turnLeft RIGHT  = UP
turnRight UP    = RIGHT
turnRight LEFT  = UP
turnRight DOWN  = LEFT
turnRight RIGHT = DOWN

turnWithInstr :: Integer -> Direction -> Direction
turnWithInstr 0 = turnLeft
turnWithInstr 1 = turnRight

day11Part1 :: IO Int
day11Part1 = do
  raw <- readBy ',' "day11.txt"
  return $ size $ robot 0 $ initIntCode $ fromList $ zip [0..] (read <$> raw)

-- day11Part2 :: IO Int
day11Part2 = do
  raw <- readBy ',' "day11.txt"
  formatImage $ robotDrawing $ robot 1 $ initIntCode $ fromList $ zip [0..] (read <$> raw)

robot :: Integer -> IntCode -> RobotPainting
robot input intCode 
  = execState (robot' intCode ((0, 0), UP) input) empty
  where
    robot' intCode pos input = do
      let (out1, ic) = runState (executeOutPut [input]) intCode
      if isNothing out1
        then return []
        else do
          panels <- get
          let (out2, ic') = runState (executeOutPut []) ic
          let pos'        = robotMove pos $ fromJust out2
          let panels'     = insert (fst pos) (fromJust out1) panels
          put panels'
          robot' ic' pos' $ maybe 0 id (panels' !? (fst pos'))

executeOutPut :: [Integer] -> State IntCode (Maybe Integer)
executeOutPut inputs = do
  segMaybe <- execOnce (headMaybe inputs)
  if isNothing segMaybe
    then return Nothing
    else do
      let Just (Segment f cV) = segMaybe
      if isJust cV
        then return cV
        else if f 
          then executeOutPut (tail inputs)
          else executeOutPut inputs

robotMove :: RobotPos -> Integer -> RobotPos
robotMove ((x, y), dir) turn
  = case dir' of
    UP    -> ((x, y - 1), dir')
    DOWN  -> ((x, y + 1), dir')
    LEFT  -> ((x - 1, y), dir')
    RIGHT -> ((x + 1, y), dir')
  where
    dir' = turnWithInstr turn dir

robotDrawing :: RobotPainting -> [String]
robotDrawing painting
  = chunk (fromIntegral $ xMax - xMin + 1) $ colourMap <$> field
  where
    colourMap p
      | (painting !? p) == Just 1 = '1'
      | otherwise                 = '0'
    field = [(x, y) |y <- [yMin..yMax], x <- [xMin..xMax]]
    pos   = keys painting
    xMin  = fst $ minimumOn fst pos
    xMax  = fst $ maximumOn fst pos
    yMin  = snd $ minimumOn snd pos
    yMax  = snd $ maximumOn snd pos
