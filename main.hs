{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Applicative hiding ((<|>))
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.String

type Identifier = String

data Expr = Var Identifier
          | Lam Identifier Expr
          | App Expr Expr
            deriving Show

-- instance Show Expr where
pp :: Either ParseError Expr -> IO ()
pp (Left e) = print e
pp (Right e) = putStrLn $ p e
  where
    p (Var x) = x
    p (Lam x t) = "\\" ++ x ++ "." ++ p t
    p (App f x) = "(" ++ p f ++ " " ++ p x ++ ")"



parseLambda :: Parser Expr
parseLambda = do
    binding <- char '\\' *> many1 letter <* char '.'
    body <- parseExpr
    return $ Lam binding body

parseVar :: Parser Expr
parseVar = Var <$> many1 letter

parseApp :: Parser Expr
parseApp = do
    f <- parseVar
    char ' '
    x <- parseExpr
    return $ App f x

parseExpr :: Parser Expr
parseExpr = try parseLambda <|> try parseApp <|> parseVar

test = parse parseExpr "lambda calculus parser"
