module Lexer where

import Tokenizer

data Lexeme
  = LxNum
  | LxVarId

type Lexer = Tokenizer Char Lexeme

