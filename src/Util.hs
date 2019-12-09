module Util where

import System.IO.Unsafe
import qualified Data.Map as M

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

(!@) :: (Num a, Ord a) => M.Map a a -> a -> a
(!@) = flip $ M.findWithDefault 0

(!@*) :: (Num a, Ord a) => M.Map a a -> a -> a
m !@* k = m !@ (m !@ k)

relativeAccess :: (Num a, Ord a) => a -> M.Map a a -> a -> a
relativeAccess r m k = m !@ ((m !@ k) + r)

(//) :: (Num a, Ord a) => M.Map a a -> [(a, a)] -> M.Map a a
m // pairs = M.union (M.fromList pairs) m
