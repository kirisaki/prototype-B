module AST where

data Exp
  = Apply Exp Exp
  | Lambda String Exp
  | Var String
