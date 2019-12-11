{-# LANGUAGE RecordWildCards #-}

module Intcode where

import Util ((!@), (!@*), relativeAccess, (//), split)

import qualified Data.Map as M
import Data.Map ((!))

type MemoryState = M.Map Integer Integer
type InstructionPointer = Integer
type Instruction = Integer

-- Operands are pairs of integers
-- The first is the parameter we should consider in read mode, the second is the parameter we should consider in write mode
type Operands = [(Integer, Integer)]

type Operation = ProgramState -> Operands -> ProgramState

data ProgramState = ProgramState
                    {mem :: MemoryState,
                     ip :: InstructionPointer,
                     relBase :: Integer,
                     input :: [Integer],
                     output :: [Integer]}
instance Show ProgramState where
  show ProgramState{..} =
    "Memory: " ++ (show mem) ++ " Instruction Pointer: " ++ (show ip) ++ " Relative Base: " ++ (show relBase) ++ " Input: " ++ (show input) ++ " Output: " ++ (show output)

data StepResult = NeedInput | Halt | UpdatedState ProgramState
instance Show StepResult where
  show s = case s of
    NeedInput -> "Need Input"
    Halt -> "Halt"
    UpdatedState p -> (show p)

--------------------------------------------------------------------------------
-- INITIALISING AND READING

-- Reads in an Intcode program
readIntcode :: String -> MemoryState
readIntcode = (\x -> M.fromList $ zip [0..] x)
  . (map read)
  . (split (==','))

-- Makes a program with the memory and input given
initialiseProgram :: MemoryState -> [Integer] -> ProgramState
initialiseProgram mem input = ProgramState mem 0 0 input []

-- Adds input to a program's input buffer
sendInput :: ProgramState -> [Integer] -> ProgramState
sendInput p@ProgramState{..} i = p {input = input ++ i}

-- Get a program's output buffer
getOutput :: ProgramState -> [Integer]
getOutput ProgramState{..} = output

-- Clears a program's output buffer
clearOutput :: ProgramState -> ProgramState
clearOutput p = p {output = []}

--------------------------------------------------------------------------------
-- OPERATIONS AND OPERANDS

-- A map that associates opcodes with operations
operationMap :: M.Map Integer Operation
operationMap = M.fromList [(1, binaryOp (+)),
                           (2, binaryOp (*)),
                           (3, inputOp),
                           (4, outputOp),
                           (5, jumpOp (/=0)),
                           (6, jumpOp (==0)),
                           (7, binaryOp $ \x y -> if x < y then 1 else 0),
                           (8, binaryOp $ \x y -> if x == y then 1 else 0),
                           (9, relBaseModOp)]

-- This gets us a list of all the numbers the operations have to think about
-- It strips away most of the parameter mode complexity
-- The first element of the tuples is the parameter as it should be considered in read mode
-- The second element of the tuples is the parameter as it should be considered in write mode
getOperandList :: Instruction -> MemoryState -> InstructionPointer -> Integer -> Operands
getOperandList instruction mem ip relBase = zipWith
                                            (\m addr -> (fst m $ addr, snd m $ addr))
                                            modes [ip+1..] where
  modes = (flip (++) $ repeat ((\k -> mem !@* k), (\k -> mem !@ k)))
          . (map (\x -> case x of
                     '0' -> ((\k -> mem !@* k), (\k -> mem !@ k))
                     '1' -> ((\k -> mem !@ k), \_ -> error "Can't use parameter mode 1 to write!")
                     '2' -> ((\k -> relativeAccess relBase mem k), (\k -> (mem !@ k) + relBase))
                     _ -> error "Invalid parameter mode!"
                 ))
          . reverse . show $ instruction `div` 100

-- Accesses the <i>th parameter, considering it in read mode
(!!<) :: Operands -> Int -> Integer
os !!< i = fst $ os !! i

-- Accesses the <i>th parameter, considering it in write mode
(!!>) :: Operands -> Int -> Integer
os !!> i = snd $ os !! i

--------------------------------------------------------------------------------
-- RUNNING INTCODE

-- A function that performs one step of a ProgramState and returns a StateResult
step :: ProgramState -> StepResult
step p@ProgramState{..}
    | instruction == 3 && input == [] = NeedInput
    | instruction >= 1 && instruction <= 9
      = UpdatedState $ (operationMap M.! instruction) p operands
    | instruction == 99 = Halt
    | otherwise = error $ "Invalid opcode " ++ (show instruction) where
  val = mem ! ip
  instruction = val `mod` 100
  operands = getOperandList val mem ip relBase

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
runIntcodeWhileInput :: ProgramState -> ([Integer], Maybe ProgramState)
runIntcodeWhileInput p = (case p' of
                            NeedInput -> (output p, Just p)
                            Halt -> (output p, Nothing)
                            UpdatedState newState -> runIntcodeWhileInput newState) where
  p' = step p

-- Given a MemoryState, runs an Intcode program with the instruction pointer set to 0
runIntcode :: MemoryState -> MemoryState
runIntcode mem' = mem . runIntcodeUntilHalt $ initialiseProgram mem' []

-- Given a MemoryState and some input, runs an Intcode program with the instruction pointer set to 0
runIntcodeWithIO :: MemoryState -> [Integer] -> [Integer]
runIntcodeWithIO mem' input' = output . runIntcodeUntilHalt $ initialiseProgram mem' input'

-- Legacy function for Day 2.
day2Output :: MemoryState -> Integer
day2Output mem = mem ! 0

--------------------------------------------------------------------------------
-- OPERATIONS
-- Operations are functions that take some numbers and return a modified ProgramState
-- To access their nth operand, we use <ops !! (n-1)>
-- Location to store the result is determined by directly accessing the parameter, every time

-- This represents a simple binary operation
-- This function takes a binary operator, and applies it to the first two parameters
-- It then stores the result in the third parameter
binaryOp :: (Integer -> Integer -> Integer) -> Operation
binaryOp (<^>) p@ProgramState{..} ops =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  loc = ops !!> 2
  val = (ops !!< 0) <^> (ops !!< 1)

-- Takes input off the input buffer and stores it in the address given by the first parameter
inputOp :: Operation
inputOp p@ProgramState{..} ops =
  p {ip = ip + 2,
     input = input',
     mem = mem // [(loc, val)]} where
  (val, input') = case input of
    (v:i) -> (v,i)
    _ -> error "Run out of input!"
  loc = ops !!> 0

-- Takes the first parameter and pushes it to the output buffer
outputOp :: Operation
outputOp p@ProgramState{..} ops =
  p {ip = ip + 2,
     output = val : output} where
  val = ops !!< 0

-- Takes a predicate and applies it to the first parameter
-- If the predicate is true, then it moves the instruction pointer to the position given by the second parameter
-- If false, simply advances the instruction pointer as normal
jumpOp :: (Integer -> Bool) -> Operation
jumpOp predicate p@ProgramState{..} ops =
  p {ip = if predicate $ (ops !!< 0) then (ops !!< 1) else ip + 3} where

-- Modifies the relative base by adding the first parameter to it
relBaseModOp :: Operation
relBaseModOp p@ProgramState{..} ops =
  p {ip = ip + 2,
     relBase = relBase + (ops !!< 0)}
