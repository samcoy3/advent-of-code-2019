module Day16 where

import Util (rawInput)

filename :: String
filename = "input/Day16.txt"

input :: [Int]
input = map (read . pure) . head . lines $ rawInput filename

-- Gets the output pattern at the <n>th position
outputPatternForPosition :: Int -> [Int]
outputPatternForPosition n = tail . cycle . concat $ map (replicate (n+1)) [0, 1, 0, (-1)]

outputPatterns :: [[Int]]
outputPatterns = map outputPatternForPosition [0..]

-- Does one step of the phaseFFT algorithm
phaseFFT :: [[Int]] -> [Int] -> [Int]
phaseFFT patterns inputList = zipWith applyPatterns inputList patterns where
  applyPatterns _ op = (flip mod 10) . abs . sum $ zipWith (*) inputList op

part1 :: Int
part1 = read . concat . map show . take 8 $ iterate (phaseFFT outputPatterns) input !! 100

messageOffset :: Int
messageOffset = read . concat . map show $ take 7 input

newInput :: [Int]
newInput = drop messageOffset . concat $ replicate 10000 input

headWithDefault :: [Int] -> Int
headWithDefault l
  | l == [] = 0
  | otherwise = head l

newPhaseFFT :: [Int] -> [Int]
newPhaseFFT inputList = foldr (\x l -> mod (x + (headWithDefault l)) 10 : l) [] inputList

-- TODO: Fix this so it doesn't take as long
part2 :: Int
part2 = read . concat . map show . take 8 $ iterate newPhaseFFT newInput !! 100
