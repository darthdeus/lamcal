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
