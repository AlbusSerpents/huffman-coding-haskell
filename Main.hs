module Main where

import           Lib
import qualified Syntax as S

main :: IO ()
main = do
  input <- getLine
  printEncoded $ encode input

printEncoded
  :: (Show a)
  => (S.Code, S.Cipher a) -> IO ()
printEncoded (code, cipher) = do
  putStrLn "endoded"
  putStrLn $ concatMap show code
  putStrLn "cipher"
  print cipher
