module Day01 where

import Util

filename :: String
filename = "input/Day01.txt"

fuelRequired :: Integer -> Integer
fuelRequired = (flip (-) 2) . (flip div 3)

input :: [Integer]
input = (map read) . lines . rawInput $ filename

part1 :: Integer
part1 = foldr (+) 0 $ map fuelRequired input

part2 :: Integer
part2 = foldr (+) 0 $ map recurseFuelRequired input where
  recurseFuelRequired x
    | fuelRequired x <= 0 = 0
    | otherwise = (fuelRequired x) + (recurseFuelRequired . fuelRequired $ x)
