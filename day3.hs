import           Control.Applicative
import           Data.List

import           InputParser

day3Parser :: String -> ([String], [String])
day3Parser str
  = let (a : [b]) = split ',' <$> lines str in (a, b)

day3Part1 :: IO Int
day3Part1 = do
  (w, w') <- day3Parser <$> readFile "day3.txt"
  return $ findClosestIntersectionDistance w w'

foo = do
  (w, w') <- day3Parser <$> readFile "day3.txt"
  return (w, w')

day3Part2 :: IO Int
day3Part2 = do
  (w, w') <- day3Parser <$> readFile "day3.txt"
  return $ findStepsToIntersection w w'

type Segment = (Int, (Int, Int))
type Wire = ([Segment], [Segment])

segments :: [String] -> Wire
segments ws
  = segments' ws 0 0
  where
    segments' [] _ _
      = ([], [])
    segments' (('D' : len) : ws) x y 
      = (xSegs, (x, (y - read len, y)) : ySegs)
      where
        (xSegs, ySegs) = segments' ws x (y - read len)
    segments' (('L' : len) : ws) x y 
      = ((y, (x - read len, x)) : xSegs, ySegs)
      where
        (xSegs, ySegs) = segments' ws (x - read len) y
    segments' (('R' : len) : ws) x y 
      = ((y, (x, x + read len)) : xSegs, ySegs)
      where
        (xSegs, ySegs) = segments' ws (x + read len) y
    segments' (('U' : len) : ws) x y 
      = (xSegs, (x, (y, y + read len)) : ySegs)
      where
        (xSegs, ySegs) = segments' ws x (y + read len)

xyInter :: [Segment] -> [Segment] -> [(Int, Int)]
xyInter [] _
      = []
xyInter (s : segs) s'
  = (inter' s s' ++ xyInter segs s') \\ [(0, 0)]
inter' _ []
  = []
inter' s@(y, (xMin, xMax)) ((x, (yMin, yMax)) : segs)
  | y < yMin  = inter' s segs
  | x < xMin  = inter' s segs
  | y > yMax  = inter' s segs
  | x > xMax  = inter' s segs
  | otherwise = (x, y) : inter' s segs


intersections :: Wire -> Wire -> [(Int, Int)]
intersections (s1, s2) (s1', s2')
  = xyInter s1 s2' ++ xyInter s1' s2

findClosestIntersectionDistance :: [String] -> [String] -> Int
findClosestIntersectionDistance ws ws'
  = minimum $ map (\(x, y) -> abs x + abs y) inters
  where   
    inters = intersections (segments ws) (segments ws')

findStepsToIntersection :: [String] -> [String] -> Int
findStepsToIntersection ws ws'
  = minimum $ map (liftA2 (+) (findSteps ws) (findSteps ws')) inters
  where
    inters = intersections (segments ws) (segments ws')

findSteps :: [String] -> (Int, Int) -> Int
findSteps ws (x, y) 
  = findSteps' 0 (0, 0) ws
  where
    findSteps' n (cX, cY) (('D' : len) : ws)
      | canReach  = n + cY - y
      | otherwise = findSteps' (n + read len) (cX, cY - read len) ws
      where
        canReach = cX == x && cY >= y && cY - read len <= y 
    findSteps' n (cX, cY) (('L' : len) : ws)
      | canReach  = n + cX - x
      | otherwise = findSteps' (n + read len) (cX - read len, cY) ws
      where
        canReach = cY == y && cX >= x && cX - read len <= x 
    findSteps' n (cX, cY) (('R' : len) : ws)
      | canReach  = n - cX + x
      | otherwise = findSteps' (n + read len) (cX + read len, cY) ws
      where
        canReach = cY == y && cX <= x && cX + read len >= x 
    findSteps' n (cX, cY) (('U' : len) : ws)
      | canReach  = n - cY + y
      | otherwise = findSteps' (n + read len) (cX, cY + read len) ws
      where
        canReach = cX == x && cY <= y && cY + read len >= y

