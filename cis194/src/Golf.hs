module Golf
  (skips) where

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
