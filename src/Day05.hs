module Day05 where

import Util (rawInput)
import Intcode (readIntcode, runIntcodeWithIO)

import Data.Array

filename :: String
filename = "input/Day05.txt"

input :: Array Int Int
input = readIntcode . rawInput $ filename

part1 :: Int
part1 = head $ runIntcodeWithIO input [1]

part2 :: Int
part2 = head $ runIntcodeWithIO input [5]
