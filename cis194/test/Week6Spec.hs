module Week6Spec
  ( spec
  ) where

import qualified Data.Map        as M
import           Test.Hspec
import           Week6.Fibonacci

spec :: Spec
spec = do
  describe "fib" $ do
    it "should return fibonacci number at given number" $ do
      fib 6 `shouldBe` 8
      fib 11 `shouldBe` 89

  describe "fibs1" $ do
    it "should return correct fibonacci series" $ do
      (take 9 $ fibs1) `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21]

  describe "fibs2" $ do
    it "should return correct fibonacci series" $ do
      (take 30 $ fibs1) `shouldBe`[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229]
