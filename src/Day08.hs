module Day08 where

import Util(rawInput)

import Data.List

type Layer = [[Int]]

filename :: String
filename = "input/Day08.txt"

-- The size of each layer
dimensions :: (Int, Int)
dimensions = (25, 6)

-- Splits a list into chunks of the size provided
chunk :: Int -> [a] -> [[a]]
chunk size list
  | length list < size = []
  | otherwise = (:) (take size list) . chunk size $ drop size list

input :: [Layer]
input = (map $ chunk (fst dimensions))  . chunk (fst dimensions * snd dimensions) . (map read) . (map pure) $ rawInput filename

-- Counts the occurrences of a number in a layer
count :: Layer -> Int -> Int
count l x = length $ filter (==x) (concat l)

-- Compares the counts of occurrences of a number between two layers
compareCounts :: Int -> Layer -> Layer -> Ordering
compareCounts x l1 l2 = compare (count l1 x) (count l2 x)

part1 :: Int
part1 = (\layer -> count layer 1 * count layer 2) . head $ sortBy (compareCounts 0) input

-- Combines two layers
-- The number 2 is see-through, so if 2 is in a spot on the top layer we use the bottom layer instead
-- Otherwise we just use the value on the top layer
combineLayers :: Layer -> Layer -> Layer
combineLayers topLayer bottomLayer = zipWith combineLines topLayer bottomLayer where
  combineLines = zipWith (\t b -> if t == 2 then b else t)

-- Converts the layer into a string
displayLayer :: Layer -> String
displayLayer l = (++) "\n" $ unlines . map concat . map (map $ \x -> if x == 1 then "■" else "□") $ l

part2 :: String
part2 = displayLayer $ foldl1 combineLayers input
