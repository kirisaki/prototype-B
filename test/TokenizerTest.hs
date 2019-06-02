module TokenizerTest where

import Tokenizer

import Test.Tasty
import Test.Tasty.HUnit

unit_satisfy :: IO ()
unit_satisfy = runTokenizer (satisfy (== 'a')) (TokenizerState [] 0) "abc" @?= Right ('a', "bc")
