module Printer (showExp) where

import Types
import Test.Hspec

showExp :: Expr -> String
showExp (Var x) = x
showExp (Lam x y) = "\\" ++ x ++ "." ++ showExp y
showExp (App x y) = showExp x ++ " " ++ maybeWrap y

maybeWrap :: Expr -> String
maybeWrap (Var x) = x
maybeWrap x = "(" ++ showExp x ++ ")"

(|@|) :: (Show a, Eq a) => a -> a -> Expectation
(|@|) = shouldBe

main :: IO ()
main = hspec $ do
    it "var printer" $ do
        showExp (Var "x") |@| "x"
        showExp (Var "hello") |@| "hello"

    it "lambda printer" $ do
        showExp (Lam "x" (Var "x")) |@| "\\x.x"
        showExp (Lam "x" (Lam "y" (Var "x"))) |@| "\\x.\\y.x"

    it "function application printer" $ do
        showExp (App (Var "f") (Var "x")) |@| "f x"
        showExp (App (App (Var "f") (Var "g")) (Var "x")) |@| "f g x"
        showExp (App (Var "f") (App (Var "g") (Var "x"))) |@| "f (g x)"
