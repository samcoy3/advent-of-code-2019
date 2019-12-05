{-# LANGUAGE RecordWildCards #-}

module Intcode where

import Util ((!*), split)

import Data.Array

type MemoryState = Array Int Int
type InstructionPointer = Int

type Instruction = Int
type Operands = [Int]

type Operation = ProgramState -> Operands -> ProgramState

data ProgramState = ProgramState
                    {mem :: MemoryState,
                     ip :: InstructionPointer,
                     input :: [Int],
                     output :: [Int]}

-- This gets us a list of all the numbers the operations have to think about
-- It strips away all of the parameter mode complexity
getOperandList :: Instruction -> MemoryState -> InstructionPointer -> Operands
getOperandList instruction mem ip = zipWith
                                (\addr mode -> mode mem addr)
                                [ip+1..] modes where
  modes = (flip (++) $ repeat (!*))
          . (map (\x -> if x == '1' then (!) else (!*)))
          . reverse . show $ instruction `div` 100

-- Reads in an Intcode program
readIntcode :: String -> MemoryState
readIntcode = (\x -> listArray (0, (length x) - 1) x)
  . (map read)
  . (split (==','))

-- Runs a ProgramState until an opcode of 99 is reached
runIntcode' :: ProgramState -> ProgramState
runIntcode' p@ProgramState{..} = (case instruction of
  1 -> runIntcode' $ addOp p operands
  2 -> runIntcode' $ multOp p operands
  3 -> runIntcode' $ inputOp p operands
  4 -> runIntcode' $ outputOp p operands
  5 -> runIntcode' $ jitOp p operands
  6 -> runIntcode' $ jifOp p operands
  7 -> runIntcode' $ lessThanOp p operands
  8 -> runIntcode' $ equalsOp p operands
  99 -> p
  _ -> error $ "Invalid opcode " ++ (show instruction)) where
  val = mem ! ip
  instruction = val `mod` 100
  operands = getOperandList val mem ip

-- Given a MemoryState, runs an Intcode program with the instruction pointer set to 0
runIntcode :: MemoryState -> MemoryState
runIntcode mem' = mem . runIntcode' $ ProgramState mem' 0 [] []

-- Given a MemoryState and some input, runs an Intcode program with the instruction pointer set to 0
runIntcodeWithIO :: MemoryState -> [Int] -> [Int]
runIntcodeWithIO mem' input' = output . runIntcode' $ ProgramState mem' 0 input' []

-- Legacy function for Day 2.
day2Output :: MemoryState -> Int
day2Output mem = mem ! 0

-- OPERATIONS
-- Operations are functions that take some numbers and return a modified ProgramState
-- To access their nth operand, we use <ops !! (n-1)>
-- Location to store the result is determined by directly accessing the parameter, every time
addOp :: Operation
addOp p@ProgramState{..} ops =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  loc = mem ! (ip + 3)
  val = ops !! 0 + ops !! 1

multOp :: Operation
multOp p@ProgramState{..} ops =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  loc = mem ! (ip + 3)
  val = ops !! 0 * ops !! 1

inputOp :: Operation
inputOp p@ProgramState{..} _ =
  p {ip = ip + 2,
     input = input',
     mem = mem // [(loc, val)]} where
  (val, input') = case input of
    (v:i) -> (v,i)
    _ -> error "Run out of input!"
  loc = mem ! (ip + 1)

outputOp :: Operation
outputOp p@ProgramState{..} ops =
  p {ip = ip + 2,
     output = val : output} where
  val = ops !! 0

jitOp :: Operation
jitOp p@ProgramState{..} ops =
  p {ip = if ops !! 0 /= 0 then ops !! 1 else ip + 3,
     input = ip : input} where

jifOp :: Operation
jifOp p@ProgramState{..} ops =
  p {ip = if ops !! 0 == 0 then ops !! 1 else ip + 3,
     input = ip : input} where

lessThanOp :: Operation
lessThanOp p@ProgramState{..} ops =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  loc = mem ! (ip + 3)
  val = if ops !! 0 < ops !! 1 then 1 else 0

equalsOp :: Operation
equalsOp p@ProgramState{..} ops =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  loc = mem ! (ip + 3)
  val = if ops !! 0 == ops !! 1 then 1 else 0
