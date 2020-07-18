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

  describe "streamToList" $ do
    it "should convert Stream to List" $ do
      (take 4 $ streamToList (Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 $ undefined)) `shouldBe` [1, 2, 3, 4]

  describe "streamMap" $ do
    it "should have correct fmap implementation" $ do
      (take 4 $ streamToList $ streamMap (* 2) (Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 $ undefined)) `shouldBe` [2, 4, 6, 8]

  describe "streamRepeat" $ do
    it "should create Stream by repeating given argument" $ do
      (take 4 $ streamToList $ streamRepeat 1) `shouldBe` [1, 1, 1, 1]

  describe "streamFromSeed" $ do
    it "should create Stream with given seed and unfolding function" $ do
      (take 4 $ streamToList $ streamFromSeed ('x':) "o") `shouldBe` ["o", "xo", "xxo", "xxxo"]

  describe "sInterleave" $ do
    it "should interleave elements from two streams" $ do
      (take 8 $ streamToList $ sInterleave (streamRepeat 0) (streamRepeat 1)) `shouldBe` [0, 1, 0, 1, 0, 1, 0, 1]

  describe "sTake" $ do
    it "should create a list of given length from stream" $ do
      (sTake 4 $ streamRepeat 1) `shouldBe` [1, 1, 1, 1]

  describe "nats" $ do
    it "should be a stream of natural numbers" $ do
      (sTake 4 nats) `shouldBe` [0, 1, 2, 3]

  describe "ruler" $ do
    it "should implement ruler function" $ do
      (sTake 10 ruler) `shouldBe` [0,1,0,2,0,1,0,3,0,1]
