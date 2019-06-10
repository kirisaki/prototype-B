module ParserTest where

import Lexer (lexemes)
import Tokenizer
import Parser
import AST

import Test.Tasty
import Test.Tasty.HUnit


unit_varId :: IO ()
unit_varId = do
  let Right (lxs, _) = runTokenizer lexemes (TokenizerState [] 0) "abc"
  runTokenizer varId (TokenizerState [] 0) lxs
    @?= Right (Var "abc" , [])

unit_varSym :: IO ()
unit_varSym = do
  let Right (lxs, _) = runTokenizer lexemes (TokenizerState [] 0) "+"
  runTokenizer varSym (TokenizerState [] 0) lxs
    @?= Right (Var "+" , [])

unit_litInt :: IO ()
unit_litInt = do
  let Right (lxs, _) = runTokenizer lexemes (TokenizerState [] 0) "1234"
  runTokenizer litInt (TokenizerState [] 0) lxs
    @?= Right (Lit $ Int 1234 , [])

unit_apply :: IO ()
unit_apply = do
  let Right (lxs, _) = runTokenizer lexemes (TokenizerState [] 0) "f 1234"
  runTokenizer apply (TokenizerState [] 0) lxs
    @?= Right (Apply (Var "f") (Lit $ Int 1234) , [])

unit_assoc :: IO ()
unit_assoc = do
  let Right (lxs, _) = runTokenizer lexemes (TokenizerState [] 0) "x+y"
  runTokenizer assoc (TokenizerState [] 0) lxs
    @?= Right (Apply (Apply (Var "+") (Var "x")) (Var "y") , [])
