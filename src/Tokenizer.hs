{-# LANGUAGe FunctionalDependencies #-}
{-# LANGUAGe FlexibleInstances #-}
{-# LANGUAGe FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Tokenizer where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Functor.Identity

type SourcePos = Int

data TokenizerError = TokenizerError
  { message :: String
  , position :: SourcePos
  } deriving (Show, Eq)

data TokenizerState i = TokenizerState
  { tokSource :: [i]
  , tokPos :: SourcePos
  }


newtype Tokenizer i a = Tokenizer
  { runTokenizer :: TokenizerState i -> [i] -> Either TokenizerError (a, [i]) }

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
  empty = Tokenizer $ \s i ->  Left $ TokenizerError "empty" 0
  p <|> q = Tokenizer $ \s i -> case runTokenizer p s i of
    Right x -> Right x
    Left _ -> runTokenizer q s i

instance Monad (Tokenizer i) where
  p >>= f = Tokenizer $ \s i -> case runTokenizer p s i of
    Right (x, o) -> runTokenizer (f x) s o
    Left e -> Left e

primToken :: (SourcePos -> i -> [i] -> SourcePos)
          -> (i -> Maybe o)
          -> Tokenizer i o
primToken = \nextpos match -> Tokenizer $ \TokenizerState{..} i ->
  case i of
    [] -> Left $ TokenizerError "empty input" tokPos
    c:cs -> case match c of
      Just x ->
        let
          newpos = nextpos tokPos c cs
          newst = TokenizerState cs newpos
        in
          Right (x, cs)
      Nothing -> Left $ TokenizerError "unmatched" tokPos

tokens0 :: Eq i
       => (SourcePos -> [i] -> SourcePos)
       -> [i]
       -> Tokenizer i [i]
tokens0 _ [] = Tokenizer $ \_ _ -> Left $ TokenizerError "unknown" 0
tokens0 nextpos ts@(tk:tks) = Tokenizer $ \TokenizerState{..} i ->
  let
    walk ts [] rs = Right (ts, rs)
    walk _ _ [] = Left $ TokenizerError "source exhausted" 0
    walk ts' (t:ts) (r:rs) =
      if r == t then walk (ts' <> [t]) ts rs else Left $ TokenizerError "unexcepted" (nextpos tokPos ts)
  in
    case i of
      [] -> Left $ TokenizerError "empty input" tokPos
      c:cs
        | c == tk -> walk [c] tks cs
        | otherwise -> Left $ TokenizerError "unexcepted" tokPos

tokens :: Eq i => [i] -> Tokenizer i [i]
tokens is = tokens0
            (\pos c -> pos + length is - 1)
            is


satisfy :: (i -> Bool) -> Tokenizer i i
satisfy f = primToken
            (\pos c _ -> pos + 1)
            (\c -> if f c then Just c else Nothing)

item :: Eq i => i -> Tokenizer i i
item i = satisfy (== i)

