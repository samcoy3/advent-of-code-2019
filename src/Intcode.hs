module Intcode where

import Util ((!*), split)

import Data.Array

type InstructionPointer = Int
type MemoryState = Array Int Int
type ProgramState = (Int, MemoryState)
type Operation = ProgramState -> ProgramState

readIntcode :: String -> MemoryState
readIntcode = (\x -> listArray (0, (length x) - 1) x)
  . (map read)
  . (split (==','))

runIntcode' :: ProgramState -> ProgramState
runIntcode' (ip, mem) = case mem ! ip of
  1 -> runIntcode' . addOp $ (ip, mem)
  2 -> runIntcode' . multOp $ (ip, mem)
  99 -> (ip, mem)
  _ -> error "Invalid opcode"

runIntcode :: MemoryState -> MemoryState
runIntcode mem = snd . runIntcode' $ (0, mem)

programOutput :: MemoryState -> Int
programOutput mem = mem ! 0

-- OPERATIONS

addOp :: Operation
addOp (ip, mem) = (ip', mem') where
  ip' = ip + 4
  value = (mem !* (ip + 1)) + (mem !* (ip + 2))
  mem' = mem // [(mem ! (ip + 3), value)]

multOp :: Operation
multOp (ip, mem) = (ip', mem') where
  ip' = ip + 4
  value = (mem !* (ip + 1)) * (mem !* (ip + 2))
  mem' = mem // [(mem ! (ip + 3), value)]
