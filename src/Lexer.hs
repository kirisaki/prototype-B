module Lexer where

import Tokenizer

import Control.Applicative
import Data.Char

data Lexeme
  = LxNum { unLxNum :: Int }
  | LxVarId { unLxVarId :: String }
  | LxVarSym{ unLxVarSym :: String }
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

varSym :: Lexer
varSym = LxVarSym . (: []) <$> oneOf "+-*/"

lexemes :: Tokenizer Char [Lexeme]
lexemes = some (skipSpace *> (digit <|> varId <|> varSym))
