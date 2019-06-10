module AST where

data Expr
  = Apply Expr Expr
  | Lambda String Expr
  | Var String
  | Lit Lit
  deriving (Eq, Show)

data Lit
  = Int Int
  deriving (Eq, Show)
