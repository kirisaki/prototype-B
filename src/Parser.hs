module Parser where

import Tokenizer
import qualified Lexer as Lx
import AST

import Control.Applicative

type Parser = Tokenizer Lx.Lexeme Expr

varId :: Parser
varId = Var . Lx.unLxVarId <$> satisfy Lx.isVarId

varSym :: Parser
varSym = Var . Lx.unLxVarSym <$> satisfy Lx.isVarSym

litInt :: Parser
litInt = Lit . Int . Lx.unLxNum <$> satisfy Lx.isNum

apply :: Parser
apply = liftA2 Apply varId (varId <|> litInt)

assoc :: Parser
assoc = liftA3 (\x op y -> Apply (Apply op x) y) (varId <|> litInt) varSym (varId <|> litInt)
