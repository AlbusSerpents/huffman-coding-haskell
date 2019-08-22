module Lib
  ( encode
  , decode
  ) where

import qualified Encoding   as E
import qualified Occurances as O
import qualified Syntax     as S

encode
  :: (Eq a)
  => [a] -> (S.Code, S.Cipher a)
encode input = E.encode (O.buildOccurenceTree input) input

decode :: S.Cipher a -> S.Code -> a
decode = undefined
