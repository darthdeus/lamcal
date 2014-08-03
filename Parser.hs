module Parser where

import Control.Applicative hiding ((<|>))

import Main
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
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

parseParenLambda :: Parser Expr
parseParenLambda = char '(' *> parseLambda <* char ')'

parseTerminal :: Parser Expr
parseTerminal = try parseVar <|> parseParenLambda

parseApp :: Parser Expr
parseApp = do
    let unpar = chainl1 parseTerminal (char ' ' *> return App)
    let par = char '(' *> unpar <* char ')'
    try par <|> unpar

parseExpr :: Parser Expr
parseExpr = do
    try parseLambda <|> try parseApp <|> parseVar

test :: String -> Either ParseError Expr
test = parse parseExpr "lambda calculus parser"
