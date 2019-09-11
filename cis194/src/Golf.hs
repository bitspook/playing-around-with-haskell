module Golf
  ( skips
  , localMaxima
  , histogram
  ) where
import           Data.List

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

numStars xs = map (stars maxFq) $ freq xs
  where
    fqs = freq xs
    maxFq = snd $ maximumBy (\x y -> snd x `compare` snd y) fqs

stars maxFq (el, fq) =
  (show el) : "=" : (take fq $ repeat "*") ++ (take (maxFq - fq) $ repeat " ")

freq :: [Int] -> [(Int, Int)]
freq xs = zip (nub sortedXs) (map (\x -> length x - 1) . group $ sortedXs)
  where
    sortedXs = sort (xs ++ [0..9])

rstrip = (reverse . dropWhile (`elem` " ") . reverse)

histogram :: [Int] -> String
histogram =
  intercalate "\n" .
  reverse . map (rstrip . intercalate "") . transpose . numStars
