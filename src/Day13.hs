module Day13 where

import Util (rawInput)
import Intcode (readIntcode, runIntcodeWhileInput, runIntcodeUntilHalt, initialiseProgram, getOutput, sendInput, clearOutput, MemoryState, ProgramState)

import qualified Data.Map as M
import Data.Maybe
import Data.List

data GameObject = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq)
numberToGameObject :: Int -> Int -> Int -> GameObject
numberToGameObject l m n = case n of
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> Paddle
  4 -> Ball
  _ -> error $ "Invalid numeric code " ++ (show n) ++ " " ++ (show l) ++ " " ++ (show m)

type Point = (Int, Int)
type Score = Int

filename :: String
filename = "input/Day13.txt"

input :: MemoryState
input = readIntcode $ rawInput filename

outputToGameObjects :: [Integer] -> (M.Map Point GameObject, Score)
outputToGameObjects output' = (\(game,s) -> (game, fromMaybe 0 s)) $ outputToGameObjects' (M.empty, Nothing) (reverse . map fromIntegral $ output') where
  outputToGameObjects' (m, s) output
    | length output >= 3 && output !! 0 == -1 && output !! 1 == 0 = outputToGameObjects' (m, Just $ output !! 2) $ drop 3 output
    | length output >= 3 = outputToGameObjects' ((M.insert (output !! 0, output !! 1) (numberToGameObject (output !! 0) (output !! 1) (output !! 2)) m), s) $ drop 3 output
    | length output == 0 = (m, s)
    | otherwise = error "Output not of length divisible by three"

howManyBlocks :: [Integer] -> Int
howManyBlocks = M.size
  . M.filter (== Block)
  . fst
  . outputToGameObjects

part1 :: Int
part1 = howManyBlocks
  . getOutput
  . runIntcodeUntilHalt $ initialiseProgram input []

makeOneMove :: (M.Map Point GameObject, Score, ProgramState) -> (M.Map Point GameObject, Score, Maybe ProgramState)
makeOneMove (g, s, p) = (flip M.union g $ fst parsedOutput, max s $ snd parsedOutput, snd advanceWithInput) where
  nextGameState = (flip M.union g) . fst . outputToGameObjects . fst . runIntcodeWhileInput . clearOutput $ sendInput p [0]
  nextBallPos = head . M.keys $ M.filter (== Ball) nextGameState
  currentPaddlePos = head . M.keys $ M.filter (== Paddle) nextGameState
  currentBallPos = if g == M.empty then (0,0) else head . M.keys $ M.filter (== Ball) g
  advanceWithInput = runIntcodeWhileInput . clearOutput $ sendInput p [fromIntegral $ if (fst currentBallPos) == (fst currentPaddlePos) && (snd currentBallPos) == (snd currentPaddlePos) -1 then 0 else signum ((fst nextBallPos) - (fst currentPaddlePos))]
  parsedOutput = outputToGameObjects . fst $ advanceWithInput

part2 :: Score
part2 = playGameUntilWin (M.empty, 0, (Just $ initialiseProgram (M.insert 0 2 input) [])) where
  playGameUntilWin (g, s, p) = case p of
    Just p' -> playGameUntilWin $ makeOneMove (g, s, p')
    Nothing -> s
