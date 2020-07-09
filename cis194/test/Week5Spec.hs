module Week5Spec
  ( spec
  ) where

import           Test.Hspec

import           Week5.Calc
import           Week5.ExprT

spec :: Spec
spec = do
  describe "eval" $ do
    it "should return correct result" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
