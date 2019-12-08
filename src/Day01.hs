module Day01 where

import Util

filename :: String
filename = "input/Day01.txt"

fuelRequired :: Int -> Int
fuelRequired = (flip (-) 2) . (flip div 3)

input :: [Int]
input = (map read) . lines . rawInput $ filename

part1 :: Int
part1 = foldr (+) 0 $ map fuelRequired input

part2 :: Int
part2 = foldr (+) 0 $ map recurseFuelRequired input where
  recurseFuelRequired x
    | fuelRequired x <= 0 = 0
    | otherwise = (fuelRequired x) + (recurseFuelRequired . fuelRequired $ x)
