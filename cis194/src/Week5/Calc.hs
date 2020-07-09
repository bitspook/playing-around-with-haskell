module Week5.Calc where

import           Week5.ExprT

eval :: ExprT -> Integer
eval x = case x of
  Lit n   -> n
  Add a b -> (eval a) + (eval b)
  Mul a b -> (eval a) * (eval b)
