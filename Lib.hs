module Lib
  ( encode
  , decode
  ) where

import qualified Encoding   as E (decode, encode)
import qualified Occurances as O (buildOccurenceTree)
import qualified Syntax     as S (Cipher, Code)
import qualified Translator as T (buildSynraxTree)

encode
  :: (Eq a)
  => [a] -> (S.Code, S.Cipher a)
encode []    = ([], [])
encode input = E.encode input $ O.buildOccurenceTree input

decode :: S.Code -> S.Cipher a -> [a]
decode []   = const []
decode code = E.decode code . T.buildSynraxTree
