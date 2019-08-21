module Lib
  ( encode
  , decode
  ) where

import qualified Encoding   as E
import qualified Occurances as O

encode
  :: (Eq a)
  => [a] -> E.Code
encode input = E.encode (O.buildOccurenceTree input) input

decode = undefined
