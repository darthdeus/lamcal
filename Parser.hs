module Parser where

import Control.Applicative hiding ((<|>))

import Main (Expr(..))
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.String

pars :: Parser a -> Parser a
pars p = try (char '(' *> p <* char ')') <|> p

lambda :: Parser Expr
lambda = do
    binding <- char '\\' *> many1 letter <* char '.'
    body <- expr
    return $ Lam binding body

var :: Parser Expr
var = Var <$> many1 letter

app :: Parser Expr
app = chainl1 terminal (char ' ' *> return App)

terminal :: Parser Expr
terminal = try var <|> lambda <|> pars expr

expr :: Parser Expr
expr = try app <|> lambda <|> var

test :: String -> Either ParseError Expr
test = parse expr "lambda calculus parser"
