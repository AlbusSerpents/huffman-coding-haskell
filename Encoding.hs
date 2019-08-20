module Encoding
  ( encode
  , Code
  ) where

import           Occurances (OccuranceTree (..))

data Symbol
  = Zero
  | One
  deriving (Eq)

instance Show Symbol where
  show Zero = "0"
  show One  = "1"

type Code = [Symbol]

type Cipher a = [(a, Code)]

encode
  :: (Eq a)
  => OccuranceTree a -> [a] -> Code
encode tree = concatMap (translate cipher)
  where
    cipher = buildEncoding [] tree

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
