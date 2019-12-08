module Day08 where

import Util(rawInput)

import Data.List

type Layer = [[Int]]

filename :: String
filename = "input/Day08.txt"

dimensions :: (Int, Int)
dimensions = (25, 6)

chunk :: Int -> [a] -> [[a]]
chunk size list
  | length list < size = []
  | otherwise = (:) (take size list) . chunk size $ drop size list

input :: [Layer]
input = (map $ chunk (fst dimensions))  . chunk (fst dimensions * snd dimensions) . (map read) . (map (\x -> [x])) $ rawInput filename

count :: Layer -> Int -> Int
count l x = length $ filter (==x) (concat l)

compareCounts :: Int -> Layer -> Layer -> Ordering
compareCounts x l1 l2
  | count l1 x < count l2 x = LT
  | count l1 x > count l2 x = GT
  | otherwise = EQ

part1 :: Int
part1 = (\layer -> count layer 1 * count layer 2) . head $ sortBy (compareCounts 0) input

combineLayers :: Layer -> Layer -> Layer
combineLayers topLayer bottomLayer = zipWith combineLines topLayer bottomLayer where
  combineLines = zipWith (\t b -> if t == 2 then b else t)

displayLayer :: Layer -> String
displayLayer = unlines . map concat . map (map show)

part2 :: String
part2 = displayLayer $ foldl1 combineLayers input
