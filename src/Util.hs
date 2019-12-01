module Util where

import System.IO.Unsafe

rawInput :: FilePath -> String
rawInput = unsafePerformIO . readFile
