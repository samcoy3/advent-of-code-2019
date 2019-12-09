module Day05 where

import Util (rawInput)
import Intcode (readIntcode, runIntcodeWithIO, MemoryState)

filename :: String
filename = "input/Day05.txt"

input :: MemoryState
input = readIntcode $ rawInput filename

part1 :: Integer
part1 = head $ runIntcodeWithIO input [1]

part2 :: Integer
part2 = head $ runIntcodeWithIO input [5]
