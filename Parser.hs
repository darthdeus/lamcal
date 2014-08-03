module Parser (test) where

import Control.Applicative hiding ((<|>))

import Types
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.String
import Test.Hspec

pars :: Parser a -> Parser a
pars p = try (char '(' *> p <* char ')') <|> p

lambda :: Parser Expr
lambda = pars $ do
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
expr = try app <|> try var <|> lambda

test :: String -> Either ParseError Expr
test = parse expr "lambda calculus parser"

testParser :: Parser a -> String -> Either ParseError a
testParser p = parse p ""

(|@|) :: (Show a, Show b, Eq b) => Either a b -> b -> IO ()
(|@|) ex y = case ex of
    Left x -> fail $ show x
    Right x -> x `shouldBe` y

main :: IO ()
main = hspec $ do
    it "app" $ do
        testParser app "f x" |@| App (Var "f") (Var "x")
        testParser app "(f x) y" |@| App (App (Var "f") (Var "x")) (Var "y")
        testParser app "f (x y)" |@| App (Var "f") (App (Var "x") (Var "y"))

    it "lambda" $ do
        testParser lambda "\\x.x" |@| Lam "x" (Var "x")
        testParser lambda "\\x.(x)" |@| Lam "x" (Var "x")

    it "var" $ do
        testParser var "x" |@| Var "x"

    it "expr" $ do
        testParser expr "\\x.x" |@| Lam "x" (Var "x")
        testParser expr "(\\x.(x))" |@| Lam "x" (Var "x")
