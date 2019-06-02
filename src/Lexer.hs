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

digit :: Lexer
digit = LxNum . read <$> some number

space :: Tokenizer Char Char
space = satisfy isSpace

skipSpace :: Tokenizer Char ()
skipSpace = many space *> pure ()

letter :: Tokenizer Char Char
letter = satisfy isAlpha

varId :: Lexer
varId = LxVarId <$> ((:) <$> letter <*> many (letter <|> number))
