module Parser where

import Tokenizer
import Lexer
import AST

import Control.Applicative

type Parser = Tokenizer Lexeme Expr

varId :: Parser
varId = Var . unLxVarId <$> satisfy isVarId

varSym :: Parser
varSym = Var . unLxVarSym <$> satisfy isVarSym

litInt :: Parser
litInt = Lit . Int . unLxNum <$> satisfy isNum

