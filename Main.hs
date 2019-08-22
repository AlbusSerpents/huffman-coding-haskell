module Main where

import           Lib.Huffman

main :: IO ()
main = do
  input <- getLine
  let encoded = encode input
  printEncoded encoded
  putStrLn $ uncurry decode encoded

printEncoded
  :: (Show b)
  => (String, b) -> IO ()
printEncoded (code, cipher) = do
  putStrLn "endoded"
  putStrLn code
  putStrLn "cipher"
  print cipher
