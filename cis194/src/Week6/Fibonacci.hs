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

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) $ streamMap f s

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x s) s2 = Cons x $ sInterleave s2 s

sTake :: Int -> Stream a -> [a]
sTake n = take n . streamToList

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = ruler' 0
  where
    ruler' n = sInterleave (streamRepeat n) (ruler' (n + 1))
