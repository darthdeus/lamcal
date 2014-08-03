module Types where

type Identifier = String

data Expr = Var Identifier
          | Lam Identifier Expr
          | App Expr Expr
            deriving (Eq, Show)
