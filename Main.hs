module Main where

import qualified Encoding   as E
import qualified Occurances as O

main :: IO ()
main = do
  input <- getLine
  putStrLn $ concatMap show $ E.encode (O.buildOccurenceTree input) input
