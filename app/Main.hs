{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import Text.Read

import qualified Day01 (part1, part2)
import qualified Day02 (part1, part2)
import qualified Day03 (part1, part2)
import qualified Day04 (part1, part2)
import qualified Day05 (part1, part2)
import qualified Day06 (part1, part2)
import qualified Day07 (part1, part2)
import qualified Day08 (part1, part2)
import qualified Day09 (part1, part2)
import qualified Day10 (part1, part2)
import qualified Day11 (part1, part2)
import qualified Day12 (part1, part2)
import qualified Day13 (part1, part2)
import qualified Day14 (part1, part2)
import qualified Day15 (part1, part2)
import qualified Day16 (part1, part2)

maxDay :: Int
maxDay = 16

class TermDisplayable a where
  term :: a -> String

instance (Show a) => TermDisplayable a where
  term = show

instance {-# OVERLAPS #-} TermDisplayable String where
  term = (++) "\n"

performSolution :: (TermDisplayable a, TermDisplayable b) => (a, b) -> IO ()
performSolution (part1, part2) = do
  putStrLn $ "Part 1: " ++ (term part1)
  putStrLn $ "Part 2: " ++ (term part2)

getNumber :: IO Int
getNumber = do
  response <- getLine
  case readMaybe response :: Maybe Int of
    Nothing -> do
      putStrLn "Please enter a valid number."
      getNumber
    Just i ->
      case or [i < 1, i > maxDay] of
        True -> do
          putStrLn $ "Please enter a number between 1 and " ++ (show maxDay) ++ "."
          getNumber
        False -> return i

main :: IO ()
main = do
  putStrLn $ "Which day do you want to see solutions for (1-" ++ (show maxDay) ++ " available)?"
  response <- getNumber
  case response of
    1 -> performSolution (Day01.part1, Day01.part2)
    2 -> performSolution (Day02.part1, Day02.part2)
    3 -> performSolution (Day03.part1, Day03.part2)
    4 -> performSolution (Day04.part1, Day04.part2)
    5 -> performSolution (Day05.part1, Day05.part2)
    6 -> performSolution (Day06.part1, Day06.part2)
    7 -> performSolution (Day07.part1, Day07.part2)
    8 -> performSolution (Day08.part1, Day08.part2)
    9 -> performSolution (Day09.part1, Day09.part2)
    10 -> performSolution (Day10.part1, Day10.part2)
    11 -> performSolution (Day11.part1, Day11.part2)
    12 -> performSolution (Day12.part1, Day12.part2)
    13 -> performSolution (Day13.part1, Day13.part2)
    14 -> performSolution (Day14.part1, Day14.part2)
    15 -> performSolution (Day15.part1, Day15.part2)
    16 -> performSolution (Day16.part1, Day16.part2)
    _ -> error "This should be unreachable."
