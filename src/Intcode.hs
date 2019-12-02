module Intcode where

import Util ((!*), split)

import Data.Array
import Data.Ix

type MemoryState = Array Int Int
type ProgramState = (Int, MemoryState)
type Operation = ProgramState -> ProgramState

readIntcode :: String -> MemoryState
readIntcode = (\x -> listArray (0, (length x) - 1) x)
  . (map read)
  . (split (==','))

runIntcode' :: ProgramState -> ProgramState
runIntcode' (p, a) = case a ! p of
  1 -> runIntcode' . addOp $ (p, a)
  2 -> runIntcode' . multOp $ (p, a)
  99 -> (p, a)
  _ -> error "Invalid opcode"

runIntcode :: MemoryState -> MemoryState
runIntcode arr = snd . runIntcode' $ (0, arr)

programOutput :: MemoryState -> Int
programOutput = (flip (!) 0)

-- OPERATIONS

addOp :: Operation
addOp (p, a) = (p', a') where
  p' = p + 4
  value = (a !* (p + 1)) + (a !* (p + 2))
  a' = a // [(a ! (p + 3), value)]

multOp :: Operation
multOp (p, a) = (p', a') where
  p' = p + 4
  value = (a !* (p + 1)) * (a !* (p + 2))
  a' = a // [(a ! (p + 3), value)]
