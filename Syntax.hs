module Syntax where

type Name = String
type Value = Int

data Expr
  = Var Name
  | Num Value
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show)

