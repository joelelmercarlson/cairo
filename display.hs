#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.18 --package "process"

import System.Environment
import System.Exit
import System.IO
import System.Process 

main :: IO ()
main = do
  run <- callCommand "gm display images/latest.png"
  putStrLn $ "run: " ++ show run
