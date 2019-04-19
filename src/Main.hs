module Main where

import Wrapper.Program
import Wrapper.Options

main :: IO ()
main = runProgram =<< parseOptions
