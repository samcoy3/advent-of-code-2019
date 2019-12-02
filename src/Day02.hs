module Day02 where

import Util (rawInput, split)

import Data.Ix
import Data.Array

filename :: String
filename = "input/Day02.txt"

input :: Array Int Int
input = (\x -> listArray (0, (length x) - 1) x) . (map read) . (split (==',')) . rawInput $ filename

runIntcode :: Array Int Int -> Array Int Int
runIntcode arr = runIntcodeFromPos 0 arr where
  runIntcodeFromPos p a = case a ! p of
    1 -> runIntcodeFromPos (p + 4) (a // [(a ! (p + 3), (a ! (a ! (p + 1))) + (a ! (a ! (p + 2))))])
    2 -> runIntcodeFromPos (p + 4) (a // [(a ! (p + 3), (a ! (a ! (p + 1))) * (a ! (a ! (p + 2))))])
    99 -> a

programOutput :: Array Int Int -> Int
programOutput = (flip (!) 0)

part1 :: Int
part1 = programOutput . runIntcode . (flip (//) [(1, 12), (2, 2)]) $ input

part2 :: Int
part2 = let injectValues a (n, v) = (a // [(1, n), (2, v)], (n, v)) in 
  (\(n, v) -> 100 * n + v)
  . snd
  . head
  . (dropWhile (\x -> (programOutput . runIntcode . fst $ x) /= 19690720))
  . (map (injectValues input)) $ [(noun, verb) | noun <- [1..100], verb <- [1..100]]
