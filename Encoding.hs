module Encoding
  ( encode
  , decode
  ) where

import           Occurances (OccuranceTree (..))
import           Syntax     (Cipher, Code, Symbol (..), SyntaxTree (..))

encode
  :: (Eq a)
  => OccuranceTree a -> [a] -> (Code, Cipher a)
encode tree info = (code, cipher)
  where
    cipher = buildEncoding [] tree
    code = concatMap (translate cipher) info

buildEncoding :: Code -> OccuranceTree a -> Cipher a
buildEncoding code (Leaf element _) = [(element, code)]
buildEncoding code (Vertex left right _) =
  buildEncoding leftCode left ++ buildEncoding rightCode right
  where
    leftCode = Zero : code
    rightCode = One : code

translate
  :: (Eq a)
  => Cipher a -> a -> Code
translate [] element = error "Can't decipher"
translate ((encoded, code):xs) element
  | encoded == element = code
  | otherwise = translate xs element

decode :: Code -> SyntaxTree a -> [a]
decode [] _ = []
decode code tree = info : decode remainging tree
  where
    (remainging, info) = decodeDataPoint code tree

decodeDataPoint :: Code -> SyntaxTree a -> (Code, a)
decodeDataPoint list (DataPoint info)         = (list, info)
decodeDataPoint [] (Branch _ _)               = error "Failed to parse"
decodeDataPoint (Zero:xs) (Branch left right) = decodeDataPoint xs left
decodeDataPoint (One:xs) (Branch left right)  = decodeDataPoint xs right
