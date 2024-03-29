module Day02 where

import Util (rawInput, (//))
import Intcode (readIntcode, runIntcode, day2Output, MemoryState)

filename :: String
filename = "input/Day02.txt"

input :: MemoryState
input = readIntcode . rawInput $ filename

part1 :: Integer
part1 = day2Output . runIntcode . (flip (//) [(1, 12), (2, 2)]) $ input

part2 :: Integer
part2 = let injectValues a (n, v) = ((n, v), a // [(1, n), (2, v)]) in 
  (\(n, v) -> 100 * n + v)
  . fst
  . head
  . (dropWhile (\x -> (day2Output . runIntcode . snd $ x) /= 19690720))
  . (map (injectValues input)) $ [(noun, verb) | noun <- [1..100], verb <- [1..100]]
