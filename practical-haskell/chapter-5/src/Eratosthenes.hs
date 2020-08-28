module Eratosthenes where

primes :: [Integer]
primes = sieve [2..]
  where
    sieve []     = []
    sieve (n:ns) = n : sieve [x | x <- ns, x `mod` n /= 0]
