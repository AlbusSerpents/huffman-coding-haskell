module Lib.Huffman
  ( encode
  , decode
  ) where

import           Control.Arrow  (first)
import qualified Lib.Encoding   as E (decode, encode)
import qualified Lib.Occurances as O (buildOccurenceTree)
import qualified Lib.Syntax     as S (Cipher, Code, Symbol (..))
import qualified Lib.Translator as T (buildSynraxTree)

encode
  :: (Eq a)
  => [a] -> (String, S.Cipher a)
encode [] = ([], [])
encode input = first (concatMap show) $ E.encode input $ O.buildOccurenceTree input

decode :: String -> S.Cipher a -> [a]
decode []   = const []
decode code = E.decode (readCode code) . T.buildSynraxTree

readCode :: String -> S.Code
readCode = map toCode
  where
    toCode '0' = S.Zero
    toCode '1' = S.One
