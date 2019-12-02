module Intcode where

import Util ((!*), split)

import Data.Array
import Data.Ix

type MemoryState = Array Int Int
type ProgramState = (Int, MemoryState)

readIntcode :: String -> MemoryState
readIntcode = (\x -> listArray (0, (length x) - 1) x)
  . (map read)
  . (split (==','))

runIntcode' :: ProgramState -> ProgramState
runIntcode' (p, a) = case a ! p of
  1 -> runIntcode' ((p + 4), (a // [(a ! (p + 3), (a !* (p + 1)) + (a !*(p + 2)))]))
  2 -> runIntcode' ((p + 4), (a // [(a ! (p + 3), (a !* (p + 1)) * (a !* (p + 2)))]))
  99 -> (p, a)
  _ -> error "Invalid opcode"

runIntcode :: MemoryState -> MemoryState
runIntcode arr = snd . runIntcode' $ (0, arr)

programOutput :: MemoryState -> Int
programOutput = (flip (!) 0)
