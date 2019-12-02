module Util where

import System.IO.Unsafe

rawInput :: FilePath -> String
rawInput = unsafePerformIO . readFile

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = (case chunk of
  [] -> []
  c -> c : split p (dropWhile p r)) where
    b = break p xs
    chunk = fst b
    r = snd b
