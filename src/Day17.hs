module Day17 where

import Util (rawInput)
import Intcode (readIntcode, initialiseProgram, runIntcodeUntilHalt, getOutput, ProgramState)

import qualified Data.Map as M
import Data.List

type Point = (Int, Int)
type ShieldMap = M.Map Point ShieldObject
data Direction = North | South | East | West deriving (Eq, Show)
data ShieldObject = Space | Scaffold deriving (Eq, Show)
data Situation = Situation {shieldMap :: ShieldMap,
                            robotLocation :: Point,
                            robotDirection :: Direction} deriving (Eq, Show)

filename :: String
filename = "input/Day17.txt"

input :: ProgramState
input = initialiseProgram (readIntcode $ rawInput filename) []

neighbours :: Point -> [Point]
neighbours (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

asciiToChar :: Int -> Char
asciiToChar 46 = '.'
asciiToChar 35 = '#'
asciiToChar 10 = '\n'
asciiToChar 60 = '<'
asciiToChar 62 = '>'
asciiToChar 94 = '^'
asciiToChar 118 = 'v'
asciiToChar _ = error "Invalid character; not robot output!"

charToDirection :: Char -> Direction
charToDirection '^' = North
charToDirection '>' = East
charToDirection 'v' = South
charToDirection '<' = West
charToDirection _ = error "Invalid character; not a direction!"

findRobotInOutput :: [String] -> (Point, Direction)
findRobotInOutput ss = (\(x, y, c) -> ((y,x), charToDirection c))
  . head
  . filter (\(_,_,ch) -> ch `elem` "^>v<")
  . concat
  . map (\l -> zipWith (\y xs -> (fst xs, y, snd xs)) (repeat $ fst l) (zip [0..] $ snd l))
  $ zip [0..] ss

programOutput :: Situation
programOutput = Situation {shieldMap = M.fromList . concat . zipWith processLine [0..] $ outputAsString,
                           robotLocation = fst robot,
                           robotDirection = snd robot} where
  outputAsString = lines . map asciiToChar . map fromIntegral . reverse . getOutput $ runIntcodeUntilHalt input
  processLine x chars = zipWith (\y c -> ((x,y), objectAt c)) [0..] chars
  objectAt c = if c == '.' then Space else Scaffold
  robot = findRobotInOutput outputAsString

part1 :: Int
part1 = sum . map (\(x,y) -> x*y) . filter neighboursScaffold $ M.keys $ M.filter (== Scaffold) (shieldMap programOutput) where
  neighboursScaffold = (\p -> all (\np -> M.findWithDefault Scaffold np (shieldMap programOutput) == Scaffold) $ neighbours p)

-- Hardcoded route for the robot
part2 :: Integer
part2 = head . getOutput . runIntcodeUntilHalt $ initialiseProgram (M.insert 0 2 . readIntcode $ rawInput filename)
  $ (intersperse 44 [65,66,65,66,67,67,66,67,66,65]) ++ [10] -- Main routine
  ++ [82,44,49,50,44,76,44,56,44,82,44,49,50,10] -- Routine A
  ++ (intersperse 44 [82,56,82,54,82,54,82,56]) ++ [10] -- Routine B
  ++ (intersperse 44 [82,56,76,56,82,56,82,52,82,52]) ++ [10] -- Routine C
  ++ [110,10] -- No video feed
