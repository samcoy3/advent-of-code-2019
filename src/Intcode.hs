{-# LANGUAGE RecordWildCards #-}

-- TODO: This needs some serious tidying up
-- Maybe rewrite in State?

module Intcode where

import Util ((!*), split)

import Data.Array

type MemoryState = Array Int Int
type Operation = ProgramState -> ParameterModes -> ProgramState

type ParameterMode = MemoryState -> Int -> Int
type ParameterModes = [ParameterMode]

data ProgramState = ProgramState
                    {mem :: MemoryState,
                     ip :: Int,
                     input :: [Int],
                     output :: [Int]}

calculateModes :: Int -> ParameterModes
calculateModes instruction = (map modeMap)
                             . (flip (++) $ repeat 0)
                             . (map $ (read :: String -> Int) . (\x -> [x]))
                             . reverse
                             . show
                             $ instruction `div` 100 where
  modeMap mode = case mode of
    0 -> (!*)
    1 -> (!)
    _ -> error "Invalid parameter mode!"

readIntcode :: String -> MemoryState
readIntcode = (\x -> listArray (0, (length x) - 1) x)
  . (map read)
  . (split (==','))

runIntcode' :: ProgramState -> ProgramState
runIntcode' p@ProgramState{..} = (case instruction of
  1 -> runIntcode' $ addOp p modes
  2 -> runIntcode' $ multOp p modes
  3 -> runIntcode' $ inputOp p modes
  4 -> runIntcode' $ outputOp p modes
  5 -> runIntcode' $ jitOp p modes
  6 -> runIntcode' $ jifOp p modes
  7 -> runIntcode' $ lessThanOp p modes
  8 -> runIntcode' $ equalsOp p modes
  99 -> p
  _ -> error $ "Invalid opcode " ++ (show (mem, ip, input, output))) where
  val = mem ! ip
  instruction = val `mod` 100
  modes = calculateModes val

runIntcode :: MemoryState -> MemoryState
runIntcode mem' = mem . runIntcode' $ ProgramState mem' 0 [] []

runIntcodeWithIO :: MemoryState -> [Int] -> [Int]
runIntcodeWithIO mem' input' = output . runIntcode' $ ProgramState mem' 0 input' []

programOutput :: MemoryState -> Int
programOutput mem = mem ! 0

getOp' :: ParameterModes -> MemoryState -> Int -> Int -> Int
getOp' modes mem ip no = (modes !! (no - 1)) mem (ip + no)

-- OPERATIONS
addOp :: Operation
addOp p@ProgramState{..} modes =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  getOp = getOp' modes mem ip
  loc = mem ! (ip + 3)
  val = getOp 1 + getOp 2

multOp :: Operation
multOp p@ProgramState{..} modes =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  getOp = getOp' modes mem ip
  loc = mem ! (ip + 3)
  val = getOp 1 * getOp 2

inputOp :: Operation
inputOp p@ProgramState{..} modes =
  p {ip = ip + 2,
     input = input',
     mem = mem // [(loc, val)]} where
  (val, input') = case input of
    (v:i) -> (v,i)
    _ -> error "Run out of input!"
  getOp = getOp' modes mem ip
  loc = mem ! (ip + 1)

outputOp :: Operation
outputOp p@ProgramState{..} modes =
  p {ip = ip + 2,
     output = val : output} where
  getOp = getOp' modes mem ip
  val = getOp 1

jitOp :: Operation
jitOp p@ProgramState{..} modes =
  p {ip = if getOp 1 /= 0 then getOp 2 else ip + 3,
     input = ip : input} where
  getOp = getOp' modes mem ip

jifOp :: Operation
jifOp p@ProgramState{..} modes =
  p {ip = if getOp 1 == 0 then getOp 2 else ip + 3,
     input = ip : input} where
  getOp = getOp' modes mem ip

lessThanOp :: Operation
lessThanOp p@ProgramState{..} modes =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  getOp = getOp' modes mem ip
  loc = mem ! (ip + 3)
  val = if getOp 1 < getOp 2 then 1 else 0

equalsOp :: Operation
equalsOp p@ProgramState{..} modes =
  p {ip = ip + 4,
     mem = mem // [(loc, val)]} where
  getOp = getOp' modes mem ip
  loc = mem ! (ip + 3)
  val = if getOp 1 == getOp 2 then 1 else 0
