module Week4WholemealSpec
  ( spec
  ) where

import           Test.Hspec

import           Week4.Wholemeal

spec :: Spec
spec = do
  describe "fun1" $ do
    it "should return correct integer" $ do
      fun1 [1, 8, 3, 4, 5, 6] `shouldBe` 48

  describe "fun2" $ do
    it "should return correct integer" $ do
      fun2 2 `shouldBe` 2
      fun2 3 `shouldBe` 40
      fun2 4 `shouldBe` 6
      fun2 8 `shouldBe` 14
