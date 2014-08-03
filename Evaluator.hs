module Evaluator where

import Types

substitute :: String -> String -> Expr -> Expr
substitute name value (Var x) | name == x = Var value
                              | otherwise = Var x
