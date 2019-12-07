{-# LANGUAGE RecordWildCards #-}

module Intcode where

import Util ((!*), split)

import Data.Array
import qualified Data.Map as M

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
data StepResult = NeedInput | Halt | UpdatedState ProgramState

-- Makes a program with the memory and input given
initialiseProgram :: MemoryState -> [Int] -> ProgramState
initialiseProgram mem input = ProgramState mem 0 input []

-- Adds input to a program's input buffer
sendInput :: ProgramState -> [Int] -> ProgramState
sendInput p@ProgramState{..} i = p {input = input ++ i}

-- Clears a program's output buffer
clearOutput :: ProgramState -> ProgramState
clearOutput p = p {output = []}

-- A map that associates opcodes with operations
operationMap :: M.Map Int Operation
operationMap = M.fromList [(1, binaryOp (+)),
                           (2, binaryOp (*)),
                           (3, inputOp),
                           (4, outputOp),
                           (5, jumpOp (/=0)),
                           (6, jumpOp (==0)),
                           (7, binaryOp $ \x y -> if x < y then 1 else 0),
                           (8, binaryOp $ \x y -> if x == y then 1 else 0)]

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

-- A function that performs one step of a ProgramState and returns a StateResult
step :: ProgramState -> StepResult
step p@ProgramState{..}
    | instruction == 3 && input == [] = NeedInput
    | instruction >= 1 && instruction <= 8
      = UpdatedState $ (operationMap M.! instruction) p operands
    | instruction == 99 = Halt
    | otherwise = error $ "Invalid opcode " ++ (show instruction) where
  val = mem ! ip
  instruction = val `mod` 100
  operands = getOperandList val mem ip

-- Runs a ProgramState until an opcode of 99 is reached
runIntcodeUntilHalt :: ProgramState -> ProgramState
runIntcodeUntilHalt p = (case p' of
                  NeedInput -> error "This program should never run out of input"
                  Halt -> p
                  UpdatedState newState -> runIntcodeUntilHalt newState) where
  p' = step p

-- Runs a ProgramState until either:
-- * The program halts
-- * The program needs input
-- Returns an ([Int], Maybe ProgramState) tuple.
-- The first element of the tuple is the current output buffer of the program
-- The second element of the tuple is Just the program (if it simply needs input), or Nothing if it has halted
runIntcodeWhileInput :: ProgramState -> ([Int], Maybe ProgramState)
runIntcodeWhileInput p = (case p' of
                            NeedInput -> (output p, Just p)
                            Halt -> (output p, Nothing)
                            UpdatedState newState -> runIntcodeWhileInput newState) where
  p' = step p

-- Given a MemoryState, runs an Intcode program with the instruction pointer set to 0
runIntcode :: MemoryState -> MemoryState
runIntcode mem' = mem . runIntcodeUntilHalt $ initialiseProgram mem' []

-- Given a MemoryState and some input, runs an Intcode program with the instruction pointer set to 0
runIntcodeWithIO :: MemoryState -> [Int] -> [Int]
runIntcodeWithIO mem' input' = output . runIntcodeUntilHalt $ initialiseProgram mem' input'

-- Legacy function for Day 2.
day2Output :: MemoryState -> Int
day2Output mem = mem ! 0

-- OPERATIONS
-- Operations are functions that take some numbers and return a modified ProgramState
-- To access their nth operand, we use <ops !! (n-1)>
-- Location to store the result is determined by directly accessing the parameter, every time

-- This represents a simple binary operation
-- This function takes a binary operator, and applies it to the first two parameters
-- It then stores the result in the third parameter
binaryOp :: (Int -> Int -> Int) -> Operation
binaryOp (<^>) p@ProgramState{..} ops =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  loc = mem ! (ip + 3)
  val = (ops !! 0) <^> (ops !! 1)

-- Takes input off the input buffer and stores it in the address given by the first parameter
inputOp :: Operation
inputOp p@ProgramState{..} _ =
  p {ip = ip + 2,
     input = input',
     mem = mem // [(loc, val)]} where
  (val, input') = case input of
    (v:i) -> (v,i)
    _ -> error "Run out of input!"
  loc = mem ! (ip + 1)

-- Takes the first parameter and pushes it to the output buffer
outputOp :: Operation
outputOp p@ProgramState{..} ops =
  p {ip = ip + 2,
     output = val : output} where
  val = ops !! 0

-- Takes a predicate and applies it to the first parameter
-- If the predicate is true, then it moves the instruction pointer to the position given by the second parameter
-- If false, simply advances the instruction pointer as normal
jumpOp :: (Int -> Bool) -> Operation
jumpOp predicate p@ProgramState{..} ops =
  p {ip = if predicate $ ops !! 0 then ops !! 1 else ip + 3} where
