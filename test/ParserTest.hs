module ParserTest where

import Lexer (lexemes)
import Tokenizer
import Parser
import AST

import Test.Tasty
import Test.Tasty.HUnit


unit_varid :: IO ()
unit_varid = do
  let Right (lxs, _) = runTokenizer lexemes (TokenizerState [] 0) "abc"
  runTokenizer varId (TokenizerState [] 0) lxs
    @?= Right (Var "abc" , [])

