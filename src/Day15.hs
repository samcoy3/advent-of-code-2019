module Day15 where

import Util (rawInput)
import Intcode (readIntcode, initialiseProgram, sendInput, runIntcodeWhileInput, ProgramState)

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List

filename :: String
filename = "input/Day15.txt"

input :: ProgramState
input = initialiseProgram (readIntcode . rawInput $ filename) []

type Point = (Int, Int)
type Distance = Int
data Direction = North | East | South | West deriving (Eq, Show)
move :: Point -> Direction -> Point
move (x,y) d = case d of
  North -> (x, y-1)
  East -> (x+1, y)
  South -> (x, y+1)
  West -> (x-1, y)
directionToInput :: Direction -> [Integer]
directionToInput d = case d of
  North -> [1]
  South -> [2]
  West -> [3]
  East -> [4]

data SquareContents = Wall
  | Empty ProgramState Distance
  | Target Distance
  | Unexplored Point Direction
instance Show SquareContents where
  show s = case s of
    Wall -> "Wall"
    Target t -> "Target, distance " ++ (show t)
    Unexplored p d -> "Unexplored at " ++ (show p) ++ ", " ++ (show d)
    Empty _ d -> "Empty (distance " ++ (show d) ++ ")"
isUnexplored :: SquareContents -> Bool
isUnexplored (Unexplored _ _) = True
isUnexplored _ = False
isTarget :: SquareContents -> Bool
isTarget (Target _) = True
isTarget _ = False
isEmpty :: SquareContents -> Bool
isEmpty (Empty _ _) = True
isEmpty _ = False

type GridMap = M.Map Point SquareContents

neighbours :: Point -> [Point]
neighbours p = map (move p) [North, South, East, West]

distance :: Point -> Distance
distance (x, y) = abs x + abs y

explore :: SquareContents -> Direction -> SquareContents
explore (Empty prog dist) dir
  | result == 0 = Wall
  | result == 1 = Empty p' (dist + 1)
  | result == 2 = Target (dist + 1)
  where
    programStep = runIntcodeWhileInput . sendInput prog $ directionToInput dir
    result = head . fst $ programStep
    p' = fromJust . snd $ programStep
explore _ _ = error "Can't explore from a non-empty square!"

touch :: Point -> SquareContents -> Direction -> SquareContents
touch point contents dir = case explore contents dir of
  Wall -> Wall
  Target d -> Target d
  _ -> Unexplored point dir

touchNeighbours :: Point -> GridMap -> GridMap
touchNeighbours point grid = M.union grid $ M.fromList $ map (\dir -> (move point dir, touch point (grid M.! point) dir)) [North, East, South, West]

exploreOneSquare :: GridMap -> GridMap
exploreOneSquare grid = touchNeighbours chosenPoint $ M.insert chosenPoint (explore (grid M.! startingPoint) direction) grid where
  chosenPair = head . sortOn (distance . fst) . M.toList $ M.filter isUnexplored grid
  chosenPoint = fst chosenPair
  (Unexplored startingPoint direction) = snd chosenPair

startingGrid :: GridMap
startingGrid = (touchNeighbours (0,0) $ M.singleton (0,0) (Empty input 0))

target :: (Point, Distance)
target = (\(p, Target d) -> (p, d)) . M.findMin . M.filter isTarget . fromJust
  . find (\g -> (M.size $ M.filter isTarget g) > 0)
  $ iterate exploreOneSquare startingGrid

part1 :: Distance
part1 = snd target

fullyExploredMap :: GridMap
fullyExploredMap = fromJust . find (\g -> (M.size $ M.filter isUnexplored g) == 0) $ iterate exploreOneSquare startingGrid

spreadOxygen :: S.Set Point -> S.Set Point -> S.Set Point
spreadOxygen emptyPoints oxygenatedPoints = S.union oxygenatedPoints toBeOxygenatedPoints where
  toBeOxygenatedPoints = S.intersection emptyPoints $ S.fromList . concat . S.elems $ S.map neighbours oxygenatedPoints

part2 :: Int
part2 = fromJust . findIndex (\x -> S.size x > S.size emptyPoints) $ iterate (spreadOxygen emptyPoints) (S.singleton $ fst target) where
  emptyPoints = S.fromList . map fst . M.toList $ M.filter isEmpty fullyExploredMap
