module Week6.Fibonacci where

fib :: Integer -> Integer
fib n = case n of
  0         -> 0
  1         -> 1
  otherwise -> fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)
