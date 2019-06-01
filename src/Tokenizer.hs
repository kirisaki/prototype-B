{-# LANGUAGe FunctionalDependencies #-}
{-# LANGUAGe FlexibleInstances #-}
{-# LANGUAGe FlexibleContexts #-}
module Tokenizer where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity

type SourcePos = Int

data TokenizeError = TokenizeError
  { message :: String
  , position :: SourcePos
  }

data TokenizerState i = TokenizerState
  { tokSource :: [i]
  , tokPos :: SourcePos
  }

class TokenizerStream i o | i -> o where
   uncons :: i -> Either TokenizeError (Maybe (o, i))

instance TokenizerStream [i] i where
    uncons []     = pure Nothing
    uncons (t:ts) = pure $ Just (t,ts)

newtype Tokenizer i a = Tokenizer
  { runTokenizer :: TokenizerState i -> [i] -> Either TokenizeError (a, [i]) }

instance Functor (Tokenizer i) where
  fmap f p = Tokenizer $ \s i -> case runTokenizer p s i of
    Right (x, o) -> Right $ (f x, o)
    Left e -> Left e

instance Applicative (Tokenizer i) where
  pure v = Tokenizer $ \s i -> Right (v, i)
  pf <*> px = Tokenizer $ \s i -> case runTokenizer pf s i of
    Right (f, o) -> runTokenizer (fmap f px) s o
    Left e -> Left e

instance Alternative (Tokenizer i) where
  empty = Tokenizer $ \s i ->  Left $ TokenizeError "empty" 0
  p <|> q = Tokenizer $ \s i -> case runTokenizer p s i of
    Right x -> Right x
    Left _ -> runTokenizer q s i

instance Monad (Tokenizer i) where
  p >>= f = Tokenizer $ \s i -> case runTokenizer p s i of
    Right (x, o) -> runTokenizer (f x) s o
    Left e -> Left e

primToken :: (TokenizerStream i o)
          => (SourcePos -> o -> i -> SourcePos)
          -> (o -> Maybe o')
          -> Tokenizer i o'
primToken = \nextPos match -> undefined
