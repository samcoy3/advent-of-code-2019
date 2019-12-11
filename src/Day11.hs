module Day11 where

import Util (rawInput)
import Intcode (readIntcode, initialiseProgram, step, StepResult(NeedInput, Halt, UpdatedState), MemoryState, ProgramState, sendInput, getOutput, clearOutput)

import qualified Data.Map as M
import Data.List
import Prelude hiding (Left, Right)

type Point = (Int, Int)

data TileColour = White | Black deriving (Eq, Show)

data Direction = Up | Right | Down | Left

turn :: Integer -> Direction -> Direction
turn i dir = case i of
  0 -> (case dir of -- Turn left
          Up -> Left
          Right -> Up
          Down -> Right
          Left -> Down)
  1 -> (case dir of -- Turn right
          Up -> Right
          Right -> Down
          Down -> Left
          Left -> Up)
  _ -> error "Invalid turn direction!"
move :: Point -> Direction -> Point
move (x, y) dir = case dir of
  Up -> (x, y + 1)
  Right -> (x + 1, y)
  Down -> (x, y - 1)
  Left -> (x - 1, y)

filename :: String
filename = "input/Day11.txt"

input :: MemoryState
input = readIntcode $ rawInput filename

-- This runs the paint job for the robot
-- It runs until it needs input, then:
-- * Reads the output in its buffer
-- * Paints
-- * Moves
-- * Repeats
-- When it halts we just return the map of the hull
runPaintJob ::
  ProgramState ->
  M.Map Point TileColour ->
  Direction ->
  Point ->
  M.Map Point TileColour
runPaintJob p hull dir loc =
  (case p' of
     Halt -> hull
     UpdatedState newp -> runPaintJob newp hull dir loc
     NeedInput -> runPaintJob
       (clearOutput . (flip sendInput [(\x -> if x == Black then 0 else 1) $ M.findWithDefault Black newLoc hull]) $ p)
       (M.insert loc colour hull)
       newDir
       newLoc
    ) where p' = step p
            output = getOutput p
            colour = if output !! 1 == 0 then Black else White
            turnDir = output !! 0
            newDir = turn turnDir dir
            newLoc = move loc newDir

part1 :: Int
part1 = M.size $ runPaintJob (initialiseProgram input [0]) M.empty Up (0,0)

printTile :: TileColour -> Char
printTile White = '■'
printTile Black = ' '

-- Given a map of the hull, print it to a string
renderAsString :: M.Map Point TileColour -> String
renderAsString m = unlines . reverse $ [[printTile $ M.findWithDefault Black (x, y) m | x <- [minX..maxX]] | y <- [minY..maxY]] where
  keys = M.keys m
  minX = fst . head $ sortOn fst keys
  minY = snd . head $ sortOn snd keys
  maxX = fst . head $ sortOn (negate . fst) keys
  maxY = snd . head $ sortOn (negate . snd) keys

part2 :: String
part2 = renderAsString $ runPaintJob (initialiseProgram input [1]) (M.singleton (0,0) White) Up (0,0)
