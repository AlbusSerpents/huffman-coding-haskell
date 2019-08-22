module Lib.Occurances
  ( buildOccurenceTree
  , OccuranceTree(..)
  ) where

import           Data.Function (on)
import           Data.List     (sort)

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

type OccuranceMap a = [(a, Int)]

buildOccurenceTree
  :: (Eq a)
  => [a] -> OccuranceTree a
buildOccurenceTree = reduceTrees . map (uncurry Leaf) . buildOccurenceMap

buildOccurenceMap
  :: (Eq a)
  => [a] -> OccuranceMap a
buildOccurenceMap = foldl addOccurance []
  where
    addOccurance [] el = [(el, 1)]
    addOccurance (x@(symbol, occurances):xs) el
      | symbol == el = (symbol, occurances + 1) : xs
      | otherwise = x : addOccurance xs el

reduceTrees
  :: (Eq a)
  => [OccuranceTree a] -> OccuranceTree a
reduceTrees [x] = x
reduceTrees trees = reduceTrees $ combineTrees t1 t2 : rest
  where
    ([t1, t2], rest) = splitAt 2 $ sort trees

combineTrees :: OccuranceTree a -> OccuranceTree a -> OccuranceTree a
combineTrees a b = Vertex a b $ addCounts a b
  where
    addCounts = (+) `on` count
