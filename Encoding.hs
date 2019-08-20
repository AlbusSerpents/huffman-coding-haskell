module Encoding where

import           Occurances (OccuranceTree (..))

data Symbol
  = Zero
  | One
  deriving (Eq)

instance Show Symbol where
  show Zero = "0"
  show One  = "1"

type Code = [Symbol]

type Encoding a = [(Code, a)]

encode
  :: (Eq a, Show a)
  => [a] -> OccuranceTree a -> Encoding a
encode _ = buildEncoding []

buildEncoding :: Code -> OccuranceTree a -> Encoding a
buildEncoding code (Leaf element _) = [(code, element)]
buildEncoding code (Vertex left right _) =
  buildEncoding leftCode left ++ buildEncoding rightCode right
  where
    leftCode = Zero : code
    rightCode = One : code
