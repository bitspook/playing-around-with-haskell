module LuhnSpec
  ( spec
  ) where

import           Test.Hspec

import           Luhn
import           Luhn.Internal

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "should convert a number to list of digits" $ do
      toDigits 1234567 `shouldBe` [1, 2, 3, 4, 5, 6, 7]
  describe "doubleEveryOther" $ do
    it "should double every other number in a list from right" $ do
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]
      doubleEveryOther [3, 4, 5, 6] `shouldBe` [6, 4, 10, 6]
      doubleEveryOther [1, 2, 3, 4, 5, 6] `shouldBe` [2, 2, 6, 4, 10, 6]
  describe "sumDigits" $ do
    context "there are single digit numbers in list" $
      -- [1, 2, 3] = 1 + 2 + 3
     do
      it "should sum them all into one number" $ do
        sumDigits [1, 2, 3, 4] `shouldBe` 10
        sumDigits [5, 6, 7, 8] `shouldBe` 26
    context "there are multi digit numbers in list" $
      -- [1, 12, 3] = 1 + 1 + 2 + 3
     do
      it "should sum individual digits before summing all list items" $ do
        sumDigits [1, 12, 3, 4] `shouldBe` 11
        sumDigits [16, 7, 12, 5] `shouldBe` 22
  describe "validate" $ do
    it "should return True for valid card number" $ do
      4012888888881881 `shouldSatisfy` validate
    it "should return False for invalid card number" $ do
      4012888888881882 `shouldNotSatisfy` validate
