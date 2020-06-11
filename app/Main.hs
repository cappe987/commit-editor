module Main where

import Lib
import System.Environment

main :: IO ()
main = do 
  args <- getArgs
  if null args then
    print "No input file"
  else 
    -- someFunc 
    startWindow $ head args
