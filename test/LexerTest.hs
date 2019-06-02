module LexerTest where

import Tokenizer
import Lexer

import Test.Tasty
import Test.Tasty.HUnit


unit_digit :: IO ()
unit_digit = runTokenizer digit (TokenizerState [] 0) "1234" @?= Right (LxNum 1234 , "")
