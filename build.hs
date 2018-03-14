#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.18 --package "process"

import System.Environment
import System.Exit
import System.IO
import System.Process 

main :: IO ()
main = do
  run <- callCommand "stack build"
  r   <- callCommand "stack exec generate-cairo 1234"
  putStrLn $ "build: " ++ show run
