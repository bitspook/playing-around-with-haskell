module GolfSpec
  ( spec
  ) where

import           Test.Hspec

import           Golf

spec :: Spec
spec = do
  describe "skips" $ do
    it "should return correct list of lists" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]
