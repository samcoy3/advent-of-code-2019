module Day03 where

import Util (split, rawInput)

import Data.Maybe
import Data.List

type Point = (Int, Int)
type Segment = (Point, Point)
type Wire = [Segment]

filename :: String
filename = "input/Day03.txt"

-- Segments are represented as "R36", "D19", etc.
-- This takes a previous point and a direction and outputs a segment containing both
parseSegment :: String -> Point -> Segment
parseSegment s p@(x,y) =case s of
  (d:no) -> (case d of
    'U' -> (p, (x, y+no'))
    'D' -> (p, (x, y-no'))
    'L' -> (p, (x-no', y))
    'R' -> (p, (x+no', y))
    _ -> error "Invalid segment representation.") where
    no' = read no
  _ -> error "Invalid segment representation."

-- This turns a line of the input into a wire, which is a series of wire segments
parseWire :: String -> Wire
parseWire s = reverse $ foldl addSegmentToList [] s' where
  s' = split (==',') s
  addSegmentToList wire seg = (parseSegment seg point) : wire where
    point = case wire of
      [] -> (0, 0)
      (x:_) -> snd x

input :: (Wire, Wire)
input = (case wires of
  (w1:w2:_) -> (w1, w2)
  _ -> error "There should be two wires.") where
  wires = (map parseWire) . lines . rawInput $ filename

-- This returns a Maybe Point equal to the point where two segments cross, if that happens
cross :: Segment -> Segment -> Maybe Point
cross (p1,p2) (p3,p4)
  | fst p1 == fst p2 && snd p3 == snd p4 = crossHV (p3,p4) (p1,p2)
  | snd p1 == snd p2 && fst p3 == fst p4 = crossHV (p1,p2) (p3,p4)
  | otherwise = Nothing where
    crossHV ((x1,y),(x2,_)) ((x,y1),(_,y2)) =
      let xIntersect = (x <= x1 && x >= x2 || x >= x1 && x <= x2)
          yIntersect = (y <= y1 && y >= y2 || y >= y1 && y <= y2)
      in if xIntersect && yIntersect
      then Just (x,y)
      else Nothing

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = (abs (x1-x2)) + (abs (y1-y2))

crossingPoints :: Wire -> Wire -> [Point]
crossingPoints w1 w2 = catMaybes $ cross <$> w1 <*> w2

part1 :: Int
part1 = minimum
  . (map (manhattanDistance (0,0)))
  $ crossingPoints w1 w2 where
  (w1,w2) = input

-- This works as follows:
-- The function gets all the segments before the point, and the segment containing the point
-- It folds over the segments before the point, adding their lengths
-- It adds on the partial segment containing the point
distanceAlongWire :: Point -> Wire -> Maybe Int
distanceAlongWire p w = case span (\s -> cross (p,p) s == Nothing) w of
  (restOfWire, partialSegment:_) -> Just $
    (foldr (+) 0 (map (\s -> manhattanDistance (fst s) (snd s)) restOfWire))
    + (manhattanDistance p $ fst partialSegment)
  _ -> Nothing

part2 :: Int
part2 = minimum
  . catMaybes
  . (map (\p -> (+) <$> (distanceAlongWire p w1) <*> (distanceAlongWire p w2)))
  $ crossingPoints w1 w2 where
  (w1,w2) = input
