{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Applicative hiding ((<|>))
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

data Expr = Var String
          | Lam String Expr
          | App Expr Expr
        deriving Show
-- instance Show Expr where
--     show (Var x) = x
--     show (Lam x t) = "\\" ++ x ++ ". " ++ show t
--     show (App f x) = "(" ++ show f ++ ") " ++ show x


parseLambda :: Parser Expr
parseLambda = do
    binding <- char '\\' *> many1 letter <* char '.'
    body <- parseExpr
    return $ Lam binding body

parseVar :: Parser Expr
parseVar = Var <$> many1 letter

parseApp :: Parser Expr
parseApp = do
    f <- parseExpr <* char ' '
    x <- parseExpr
    return $ App f x


parseExpr :: Parser Expr
parseExpr = try parseApp <|> try parseLambda <|> parseVar

test = parse parseExpr "lambda calculus parser"
