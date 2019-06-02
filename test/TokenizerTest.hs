module TokenizerTest where

import Tokenizer
import Control.Applicative

import Test.Tasty
import Test.Tasty.HUnit

unit_satisfy :: IO ()
unit_satisfy = runTokenizer (satisfy (== 'a')) (TokenizerState [] 0) "abc" @?= Right ('a', "bc")

unit_satisfy_fail :: IO ()
unit_satisfy_fail = runTokenizer (satisfy (== 'a')) (TokenizerState [] 0) "bca" @?= Left (TokenizerError "unmatched" 0)

unit_many :: IO ()
unit_many = runTokenizer (many (satisfy (== 'a'))) (TokenizerState [] 0) "aaab" @?= Right ("aaa", "b")

unit_tokens :: IO ()
unit_tokens = runTokenizer (tokens "abcd") (TokenizerState [] 0) "abcdefg" @?= Right ("abcd", "efg")

unit_tokens_fail :: IO ()
unit_tokens_fail = runTokenizer (tokens "abcd") (TokenizerState [] 0) "abcf" @?= Left (TokenizerError "unexcepted" 3)
