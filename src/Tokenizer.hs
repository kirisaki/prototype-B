module Tokenizer where

import Control.Applicative

newtype Tokenizer i a = Tokenizer { runTokenizer :: [i] -> Either String (a, [i]) }

instance Functor (Tokenizer i) where
  fmap f p = Tokenizer $ \i -> case runTokenizer p i of
    Right (x, o) -> Right $ (f x, o)
    Left e -> Left e

instance Applicative (Tokenizer i) where
  pure v = Tokenizer $ \i -> Right (v, i)
  pf <*> px = Tokenizer $ \i -> case runTokenizer pf i of
    Right (f, o) -> runTokenizer (fmap f px) o
    Left e -> Left e

instance Alternative (Tokenizer i) where
  empty = Tokenizer . const $ Left "fail"
  p <|> q = Tokenizer $ \i -> case runTokenizer p i of
    Right x -> Right x
    Left _ -> runTokenizer q i

instance Monad (Tokenizer i) where
  p >>= f = Tokenizer $ \i -> case runTokenizer p i of
    Right (x, o) -> runTokenizer (f x) o
    Left e -> Left e
