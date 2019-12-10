module Day10 where

import Util (rawInput, coOrdsOf)

import Data.Ratio
import Data.List

type Point = (Int, Int)

-- Direction works as follows here:
-- Suppose we want to find the direction of p' from p, the direction is:
-- * If they are the same point, then Id
-- * If p' is directly above p, then Polarity ASC (directly below -> Polarity DSC)
-- * Otherwise if p' is to the right of p then the polarity is ASC (else DSC), and we also have the ratio of the difference vector
-- Direction is ordered in the order of the laser, with Id taking up position zero (so the array of destruction is essentially 1-indexed)
data Polarity = ASC | DSC deriving (Eq, Show)
data Direction = Direction (Ratio Int) Polarity | Polarity Polarity | Id deriving (Show, Eq)
instance Ord Direction where
  Id <= _ = True
  Polarity ASC <= _ = True
  Direction _ ASC <= Direction _ DSC = True
  Direction r ASC <= Direction r' ASC = r <= r'
  Direction r DSC <= Direction r' DSC = r <= r'
  Direction _ ASC <= Polarity DSC = True
  Polarity DSC <= Direction _ DSC = True
  _ <= _ = False

directionFrom :: Point -> Point -> Direction
directionFrom (x, y) (x', y')
  | x == x' && y == y' = Id
  | x == x' && y < y' = Polarity DSC
  | x == x' && y > y' = Polarity ASC
  | otherwise = Direction ((y'-y) % (x'-x)) (if x'>x then ASC else DSC)

filename :: String
filename = "input/Day10.txt"

input :: [Point]
input = coOrdsOf '#' . lines $ rawInput filename

-- Gets the best station in terms of asteroid coverage, along with the number of asteroids it covers
bestStation :: (Point, Int)
bestStation = head
  . sortBy (\a b -> compare (snd b) (snd a))
  . map (\(p,ds) -> (p, length ds))
  $ map (\p -> (p, nub $ map (directionFrom p) input)) input

part1 :: Int
part1 = snd bestStation - 1

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

part2 :: Int
part2 = (\(x,y) -> 100 * x + y)
  . (flip (!!) 200)
  . concat
  . transpose
  . map (sortOn $ distance (fst bestStation))
  $ [filter (\point -> directionFrom (fst bestStation) point == dir) input | dir <- sort . nub $ map (directionFrom $ fst bestStation) input]
