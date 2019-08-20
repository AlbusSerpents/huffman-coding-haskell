module Main where

import           Data.Function (on)
import           Data.List     (sort)

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

data OccuranceTree a
  = Leaf { element :: a
         , count   :: Int}
  | Vertex { left  :: OccuranceTree a
           , right :: OccuranceTree a
           , count :: Int}
  deriving (Eq, Show)

instance (Eq a) =>
         Ord (OccuranceTree a) where
  compare = compare `on` count

buildOccurenceTree
  :: (Eq a)
  => OccuranceMap a -> OccuranceTree a
buildOccurenceTree = reduceTrees . map (uncurry Leaf)

reduceTrees
  :: (Eq a)
  => [OccuranceTree a] -> OccuranceTree a
reduceTrees [x] = x
reduceTrees trees = reduceTrees $ combineTrees t1 t2 : rest
  where
    ([t1, t2], rest) = splitAt 2 $ sort trees

combineTrees
  :: (Eq a)
  => OccuranceTree a -> OccuranceTree a -> OccuranceTree a
combineTrees left right = Vertex left right $ sumCounts left right
  where
    sumCounts = (+) `on` count
