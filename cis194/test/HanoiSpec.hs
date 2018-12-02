module HanoiSpec
  ( spec
  ) where

import           Hanoi
import           Test.Hspec

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "should return correct moves" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
      length (hanoi 15 "a" "b" "c") `shouldBe` 32767
