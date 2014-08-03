module Printer (showExp) where

import Types
import Test.Hspec

showExp :: Expr -> String
showExp (Var x) = x
showExp (Lam x y) = "\\" ++ x ++ "." ++ showExp y
showExp (App x y) = wrapLeft x ++ " " ++ wrapRight y

wrapLeft :: Expr -> String
wrapLeft x@(Lam _ _) = "(" ++ showExp x ++ ")"
wrapLeft x = showExp x

wrapRight :: Expr -> String
wrapRight (Var x) = x -- TODO - only app
wrapRight x = "(" ++ showExp x ++ ")"

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
