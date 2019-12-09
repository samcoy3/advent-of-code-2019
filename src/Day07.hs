module Day07 where

import Util (rawInput)
import Intcode (readIntcode, runIntcodeWithIO, runIntcodeWhileInput, MemoryState, ProgramState, initialiseProgram, sendInput, clearOutput)

import Data.List

filename :: String
filename = "input/Day07.txt"

inputProgram :: MemoryState
inputProgram = readIntcode . rawInput $ filename

-- Just folds over the list of amplifiers, using the "signals" as the accumulating value in the fold
runAmplifiers :: MemoryState -> [Integer] -> [Integer] -> [Integer]
runAmplifiers mem input' phases = foldl (\io phase -> runIntcodeWithIO mem $ phase:io) input' phases

part1 :: Integer
part1 = maximum . (map head) $ [runAmplifiers inputProgram [0] phases | phases <- permutations [0..4]]

-- Given a list of integers, makes a list of programs with the input program as the memory, and the list, element-wise, as the input to each
initialiseAmplifiers :: [Integer] -> [ProgramState]
initialiseAmplifiers = map $ initialiseProgram inputProgram . pure

-- Runs a list of programs in the following way:
-- * Run the head of the list until either:
--   * It is exhausted (needs input that it does not have)
--   * It halts (in which case we remove it from the list)
-- * Then (if it is just polling for input) put the program at the back of the list, take its output, and feed it to the next program
-- * Returns the output of the final program remaining
-- NB: This implementation only works if the final program to finish is "E"
-- For my input (and all those I've seen) it is, but this is not specified
-- This implementation is technically incorrect (it closes the communication loop; if say "B" were removed then "A" would feed output to "C")
-- But within the scope of the question this seems not to matter
runUntilExhausted :: [Integer] -> [ProgramState] -> [Integer]
runUntilExhausted _ [] = error "Can't run zero programs!"
runUntilExhausted i (p:[]) = fst $ runIntcodeWhileInput (sendInput p i)
runUntilExhausted i (p:ps) = case runIntcodeWhileInput (sendInput p i) of
  (out, Nothing) -> runUntilExhausted (reverse out) ps
  (out, Just p') -> runUntilExhausted (reverse out) (ps ++ [clearOutput p'])

part2 :: Integer
part2 = maximum . (map head) $ [runUntilExhausted [0] $ initialiseAmplifiers perm | perm <- permutations [5..9]]
