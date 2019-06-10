module Parser where

import Tokenizer
import Lexer
import AST

import Control.Applicative

type Parser = Tokenizer Char Expr

term :: Parser
term = Var . unLxVarId <$> varId
