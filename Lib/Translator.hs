module Lib.Translator
  ( buildSynraxTree
  ) where

import           Control.Arrow (second, (***))
import           Data.List     (sortOn, span)
import           Lib.Syntax    (Cipher, Symbol (..), SyntaxTree (..))

instance Ord Symbol where
  compare One Zero = GT
  compare Zero One = LT
  compare _ _      = EQ

buildSynraxTree :: Cipher a -> SyntaxTree a
buildSynraxTree [(info, code)] = DataPoint info
buildSynraxTree cipher =
  uncurry Branch $
  onPair buildSynraxTree $
  onPair removeFirst $ span startsWithZero $ sortOn snd cipher
  where
    startsWithZero = (== Zero) . head . snd
    removeFirst = map (second tail)

onPair :: (a -> b) -> (a, a) -> (b, b)
onPair f = f *** f

combine :: SyntaxTree a -> SyntaxTree a -> SyntaxTree a
combine = Branch
