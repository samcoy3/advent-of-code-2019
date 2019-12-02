module Intcode where

import Util ((!*), split)

import Data.Array
import Data.Ix

readIntcode :: String -> Array Int Int
readIntcode = (\x -> listArray (0, (length x) - 1) x)
  . (map read)
  . (split (==','))

runIntcode :: Array Int Int -> Array Int Int
runIntcode arr = runIntcodeInstruction 0 arr where
  runIntcodeInstruction p a = case a ! p of
    1 -> runIntcodeInstruction (p + 4) (a // [(a ! (p + 3), (a !* (p + 1)) + (a !*(p + 2)))])
    2 -> runIntcodeInstruction (p + 4) (a // [(a ! (p + 3), (a !* (p + 1)) * (a !* (p + 2)))])
    99 -> a
    _ -> error "Invalid opcode"

programOutput :: Array Int Int -> Int
programOutput = (flip (!) 0)
