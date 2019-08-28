module Golf
  (skips, localMaxima) where

takeEvery :: [x] -> Int -> [x]
takeEvery xs 0 = xs
takeEvery [] _ = []
takeEvery xs n
  | length xs <= n = []
  | otherwise = head dropped : takeEvery (tail dropped) n
    where
      dropped = drop n xs

skips :: [a] -> [[a]]
skips xs = skips_ 0
  where
    skips_ i
      | i == length xs = []
      | otherwise = takeEvery xs i : skips_ (i + 1)

localMaxima :: [Integer] -> [Integer]
localMaxima as =
  case as of
    [] -> []
    [x] -> []
    (a:b:[]) -> []
    (a:b:c:xs) ->
      if b > a && b > c
        then b : (localMaxima (c : xs))
        else localMaxima (b : c : xs)
