module Main where

import           Data.Foldable

main :: IO ()
main = do
  putStrLn $ show result

result :: Integer
result = foldl' (*) 1 [1..100000]
