module ParserTest where

import Tokenizer
import Lexer
import Parser
import AST

import Test.Tasty
import Test.Tasty.HUnit


unit_varid :: IO ()
unit_varid =
  runTokenizer term (TokenizerState [] 0) "abc"
  @?= Right (Var "abc" , "")

