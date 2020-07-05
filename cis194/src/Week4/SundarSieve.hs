module Week4.SundarSieve where

import           Data.List

sundarSieve :: Integer -> [Integer]
sundarSieve n = primes
  where
    exclusions = [k | i <- [1..n], j <- [1..i], let k = i + j + (2 * i * j), k < n]
    primes = [i * 2 + 1 | i <- [1..(n - 1)], not $ elem i exclusions]
