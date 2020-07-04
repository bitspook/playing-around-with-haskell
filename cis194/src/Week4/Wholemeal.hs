module Week4.Wholemeal where

fun1 :: [Integer] -> Integer
fun1 = foldr (\x y -> (x - 2) * y) 1 . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)
