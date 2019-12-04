module Day04 where

import Data.List

input :: (Int, Int)
input = (128392, 643281)

len :: Int
len = length . show . fst $ input

-- Generates all numbers of length <l'> whose digits are in ascending order
-- Notably, the list it returns is *also* in ascending order
generateAscNumbers :: Int -> [Int]
generateAscNumbers l' = map read $ generateAscStrings l' ['1'..'9'] where
  generateAscStrings 1 ran = [[x] | x <- ran]
  generateAscStrings l ran = foldr (++) [] [ map ((:) x) $ generateAscStrings (l-1) [x..'9'] | x <- ran ]

-- Returns a sublist, where all elements are within a range, of a list
-- Importantly, this function's second argument (the list) *must be sorted*
-- But our generator's list is, so it's fine
clampToRange :: (Int, Int) -> [Int] -> [Int]
clampToRange (a, b) = (takeWhile (<=b)) . (dropWhile (<a))

-- Checks whether a number contains any consecutive, identical digits
-- i.e. the precise opposite of the number being comprised of unique digits
consecutive :: Int -> Bool
consecutive = (any (>1)) . (map length) . (groupBy (==)) . show

-- Checks whether a number contains a pair of numbers
containsPair :: Int -> Bool
containsPair = (elem 2) . (map length) . (groupBy (==)) . show

part1 :: Int
part1 = length
        . (clampToRange input)
        . (filter consecutive)
        $ generateAscNumbers len

part2 :: Int
part2 = length
        . (clampToRange input)
        . (filter containsPair)
        $ generateAscNumbers len
