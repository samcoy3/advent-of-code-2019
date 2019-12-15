module Util where

import System.IO.Unsafe
import qualified Data.Map as M

rawInput :: FilePath -> String
rawInput = unsafePerformIO . readFile

-- Returns the co-ordinates (with 0,0 in the top left), of the character in the array of strings
coOrdsOf :: Char -> [String] -> [(Int, Int)]
coOrdsOf c ss = map (\(x,y,_) -> (x,y))
  . filter (\(_,_,ch) -> ch == c)
  . concat
  . map (\l -> zipWith (\y xs -> (fst xs, y, snd xs)) (repeat $ fst l) (zip [0..] $ snd l))
  $ zip [0..] ss

-- Splits a list by a predicate, returning consecutive chunks of the list where the predicate is not true
split :: (a -> Bool) -> [a] -> [[a]]
split p xs
  | length xs == 0 = []
  | length c == 0 = split p (dropWhile p r)
  | otherwise = c : split p (dropWhile p r) where
    b = break p xs
    c = fst b
    r = snd b

(!@) :: (Num a, Ord a) => M.Map a a -> a -> a
(!@) = flip $ M.findWithDefault 0

(!@*) :: (Num a, Ord a) => M.Map a a -> a -> a
m !@* k = m !@ (m !@ k)

relativeAccess :: (Num a, Ord a) => a -> M.Map a a -> a -> a
relativeAccess r m k = m !@ ((m !@ k) + r)

(//) :: (Num a, Ord a) => M.Map a a -> [(a, a)] -> M.Map a a
m // pairs = M.union (M.fromList pairs) m
