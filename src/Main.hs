module Main where

import Wrapper.Program

main :: IO ()
main = runProgram =<< parseOptions
