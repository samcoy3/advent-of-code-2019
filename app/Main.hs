module Main where

import Text.Read

import qualified Day01 (part1, part2)
import qualified Day02 (part1, part2)
import qualified Day03 (part1, part2)

maxDay :: Int
maxDay = 3

performSolution :: (Show a, Show b) => (a, b) -> IO ()
performSolution (part1, part2) = do
  putStrLn $ "Part 1: " ++ (show part1)
  putStrLn $ "Part 2: " ++ (show part2)

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
    _ -> error "This should be unreachable."
