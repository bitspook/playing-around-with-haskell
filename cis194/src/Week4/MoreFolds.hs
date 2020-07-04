module Week4.MoreFolds where

xor :: [Bool] -> Bool
xor = foldr (\_ b -> not b) False . filter (== True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a: b) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
