module Syntax
  ( Symbol(..)
  , SyntaxTree(..)
  , Cipher
  , Code
  ) where

data Symbol
  = Zero
  | One
  deriving (Eq)

data SyntaxTree a
  = DataPoint { info :: a}
  | Branch { left  :: SyntaxTree a
           , right :: SyntaxTree a}
  deriving (Show, Eq)

instance Show Symbol where
  show Zero = "0"
  show One  = "1"

type Code = [Symbol]

type Cipher a = [(a, Code)]
