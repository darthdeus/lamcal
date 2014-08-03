module Church where

import Types

c0 :: Expr
c0 = Lam "z" (Lam "s" (Var "z"))

c1 :: Expr
c1 = Lam "z" (Lam "s" (App (Var "s") (Var "z")))

true :: Expr
true = Lam "t" (Lam "f" (Var "t"))

false :: Expr
false = Lam "t" (Lam "f" (Var "f"))

omega :: Expr
omega = App f f where f = Lam "x" (App (Var "x") (Var "x"))

pair, fst, snd :: Expr
pair = Lam "x" (Lam "y" (Lam "p" (App (App (Var "p") (Var "x")) (Var "y"))))
fst = true
snd = false
