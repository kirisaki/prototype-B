module Parser where

import Control.Applicative

newtype Parser i a = Parser { runParser :: [i] -> Either String (a, [i]) }

parse :: Parser i a -> [i] -> Either String a
parse p i = fst <$> runParser p i

item :: Parser Char Char
item = Parser $ \case
  [] -> Left "empty"
  (x:xs) -> Right (x, xs)

instance Functor (Parser i) where
  fmap f p = Parser $ \i -> case runParser p i of
    Right (x, o) -> Right $ (f x, o)
    Left e -> Left e

instance Applicative (Parser i) where
  pure v = Parser $ \i -> Right (v, i)
  pf <*> px = Parser $ \i -> case runParser pf i of
    Right (f, o) -> runParser (fmap f px) o
    Left e -> Left e

instance Alternative (Parser i) where
  empty = Parser . const $ Left "fail"
  p <|> q = Parser $ \i -> case runParser p i of
    Right x -> Right x
    Left _ -> runParser q i

instance Monad (Parser i) where
  p >>= f = Parser $ \i -> case runParser p i of
    Right (x, o) -> runParser (f x) o
    Left e -> Left e
