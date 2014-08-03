{-# LANGUAGE GADTs #-}
module Main where

type Identifier = String

data Expr = Var Identifier
          | Lam Identifier Expr
          | App Expr Expr
            deriving Show

pp :: Show a => Either a Expr -> IO ()
pp (Left e) = print e
pp (Right e) = putStrLn $ p e
  where
    p (Var x) = x
    p (Lam x t) = "\\" ++ x ++ "." ++ p t
    p (App f x) = "(" ++ p f ++ " " ++ p x ++ ")"
