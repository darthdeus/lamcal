{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import Types

pp = putStrLn . p

isApp :: Expr -> Bool
isApp (App _ _) = True
isApp _ = False

paren :: Expr -> String
paren x = if isApp x then "(" ++ p x ++ ")" else p x

p :: Expr -> String
p (Var x) = x
p (Lam x t) = "\\" ++ x ++ "." ++ paren t
p (App f x) = paren f ++ " " ++ paren x

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

pair, fst, snd :: Expr
pair = Lam "x" (Lam "y" (Lam "p" (App (App (Var "p") (Var "x")) (Var "y")))) -- "\x y p. p x y"
fst = true
snd = false
