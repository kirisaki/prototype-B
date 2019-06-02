module Lexer where

import Tokenizer

import Control.Applicative
import Data.Char

data Lexeme
  = LxNum Int
  | LxVarId String
  | LxVarSym String
  deriving (Show, Eq)

type Lexer = Tokenizer Char Lexeme

number :: Tokenizer Char Char
number = satisfy isNumber

digit :: Tokenizer Char Lexeme
digit = LxNum . read <$> some number
