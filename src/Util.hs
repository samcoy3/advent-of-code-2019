module Util where

import System.IO.Unsafe
import Data.Array

rawInput :: FilePath -> String
rawInput = unsafePerformIO . readFile

-- Splits a list by a predicate, returning consecutive chunks of the list where the predicate is not true
split :: (a -> Bool) -> [a] -> [[a]]
split p xs = (case chunk of
  [] -> []
  c -> c : split p (dropWhile p r)) where
    b = break p xs
    chunk = fst b
    r = snd b

-- This infix operator gets the element at the position given by the value at p
-- Useful for Intcode operators
(!*) :: (Ix a) => Array a a -> a -> a
arr !* p = (arr ! (arr ! p))
