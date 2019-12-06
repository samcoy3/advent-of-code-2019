module Day06 where

import Util (rawInput, split)

import Data.Tree
import Data.List

filename :: String
filename = "input/Day06.txt"

-- Given a root and an edgelist, we build a tree
buildTreeFromEdgelist :: (Eq a) => a -> [(a, a)] -> Tree a
buildTreeFromEdgelist root edges = unfoldTree buildNode root where
  buildNode x = (x, [ (snd edge) | edge <- edges, x == fst edge ])

input :: Tree String
input = buildTreeFromEdgelist "COM" edgelist where
  edgelist = (map (\x -> case x of
                      x1:x2:_ -> (x1,x2)
                      _ -> error "Somehow the edge has too few elements"))
             . (map $ split (==')')) . lines . rawInput $ filename

part1 :: Int
part1 = sum . (zipWith (*) [0..]) . map length $ levels input

-- Given two nodes and a tree, find the minimal subtree that contains both as children
-- Note that when we call this function, we know the nodes have a common ancestor
findMinimalSubtree :: (Eq a) => a -> a -> Tree a -> Tree a
findMinimalSubtree x y tree = case subForest tree of
  [] -> error "We know the inputs have a common ancestor, so this should not happen"
  children -> if validChildren == [] then tree else findMinimalSubtree x y $ head validChildren where
    validChildren = [t | t <- children, elem x $ flatten t, elem y $ flatten t]

-- Given a list of lists and an element to find, returns the index of the first list in which the element appears
findInSubLists :: (Eq a) => [[a]] -> a -> Maybe Int
findInSubLists lists item = elemIndex (head . dropWhile (not . (\l -> item `elem` l)) $ lists) lists

part2 :: Int
part2 = (case distance of
    Just x -> x - 2
    Nothing -> error "This shouldn't be Nothing!") where
  levelTree = levels $ findMinimalSubtree "SAN" "YOU" input
  distance = (+) <$> findInSubLists levelTree "SAN" <*> findInSubLists levelTree "YOU"
