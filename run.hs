#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.18 --package "process"
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do
  xs  <- getArgs
  let cmd = "stack --system-ghc exec generate-cairo"
  run <- callCommand cmd
  putStrLn $ show run
