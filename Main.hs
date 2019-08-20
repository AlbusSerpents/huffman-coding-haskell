module Main where

main :: IO ()
main = putStrLn "Hello World"

type OccuranceMap a = [(a, Int)]

makeOccurenceMap
  :: (Eq a)
  => [a] -> OccuranceMap a
makeOccurenceMap = foldl find []
  where
    find [] element = [(element, 1)]
    find (x@(symbol, count):xs) element
      | symbol == element = (symbol, count + 1) : xs
      | otherwise = x : find xs element
