module Luhn.Internal
  ( toDigits,
    doubleEveryOther,
    sumDigits )
where

toDigits :: Integer -> [Integer]
toDigits ns = reverse (digitize ns)
  where
    digitize ms
      | ms <= 0 = []
      | otherwise = ms `rem` 10 : digitize (ms `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse
  where
    doubleEveryOtherFromLeft xs = case xs of
      []     -> []
      a:[]   -> a:[]
      a:b:ns -> a:b*2:doubleEveryOtherFromLeft ns

sumDigits :: [Integer] -> Integer
sumDigits ns =
  case ns of
    []   -> 0
    x:xs -> sum (toDigits x) + sumDigits xs
