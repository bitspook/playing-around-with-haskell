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

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a:(streamToList s)

instance Functor Stream where
  fmap f (Cons a s) = Cons (f a) $ fmap f s

sRepeat :: a -> Stream a
sRepeat a = Cons a $ sRepeat a

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a $ sIterate f (f a)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x s) s2 = Cons x $ sInterleave s2 s

sTake :: Int -> Stream a -> [a]
sTake n = take n . streamToList
