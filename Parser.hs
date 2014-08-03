module Parser where

import Control.Applicative hiding ((<|>))

import Main
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

pp :: Show a => Either a Expr -> IO ()
pp (Left e) = print e
pp (Right e) = putStrLn $ p e

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
