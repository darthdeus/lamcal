{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

type Identifier = String

data Expr = Var Identifier
          | Lam Identifier Expr
          | App Expr Expr
            deriving Show

pp = putStrLn . p

p :: Expr -> String
p (Var x) = x
p (Lam x t) = "\\" ++ x ++ "." ++ p t
p (App f x) = "(" ++ p f ++ ") (" ++ p x ++ ")"

c0 :: Expr
c0 = Lam "z" (Lam "s" (Var "z"))

c1 :: Expr
c1 = Lam "z" (Lam "s" (App (Var "s") (Var "z")))

true :: Expr
true = Lam "t" (Lam "f" (Var "t"))

false :: Expr
false = Lam "t" (Lam "f" (Var "f"))

omega :: Expr
omega = App f f where f = Lam "x" (App (Var "x") (Var "x"))
