module Main where

import qualified Occurances as O

main :: IO ()
main = do
  input <- getLine
  print $ O.buildOccurenceTree input
