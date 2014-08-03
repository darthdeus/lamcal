{-# LANGUAGE GADTs #-}
module Main where

data Expr = Var String
          | Lam String Expr
          | App Expr Expr

instance Show Expr where
    show (Var x) = x
    show (Lam x t) = "\\" ++ x ++ ". " ++ show t
    show (App f x) = "(" ++ show f ++ ") " ++ show x
