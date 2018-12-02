module Hanoi
  ( hanoi
  ) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = reverse $ hanoi_ n a b c
  where
    hanoi_ 0 _ _ _ = []
    hanoi_ n a b c =  hanoi_ (n - 1) c b a ++ (a, b) : hanoi_ (n - 1) a c b
