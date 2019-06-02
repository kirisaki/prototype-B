module LexerTest where

import Tokenizer
import Lexer

import Test.Tasty
import Test.Tasty.HUnit


unit_digit :: IO ()
unit_digit = runTokenizer digit (TokenizerState [] 0) "1234" @?= Right (LxNum 1234 , "")

unit_skip_space :: IO ()
unit_skip_space = runTokenizer (skipSpace *> digit) (TokenizerState [] 0) "    1234" @?= Right (LxNum 1234 , "")

unit_varId :: IO ()
unit_varId = runTokenizer varId (TokenizerState [] 0) "ab12cd" @?= Right (LxVarId "ab12cd" , "")
