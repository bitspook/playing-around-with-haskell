module Exercise42Spec where

import qualified Data.Map   as M
import           Exercise42
import           Test.Hspec

spec :: Spec
spec = do
  describe "insert" $ do
    it "should have same behavior as Map.insert" $
      insert "Value" 40 (M.empty) `shouldBe` M.insert "Value" 40 (M.empty)

  describe "delete" $ do
    it "should have same behavior as Map.delete" $
      delete "Value"  (M.fromList [("Value", 40)]) `shouldBe` M.delete "Value" (M.fromList [("Value", 40)])

  describe "adjust" $ do
    it "should have same behavior as Map.adjust" $
      adjust (+4) "Value" (M.fromList [("Value", 40)]) `shouldBe` M.adjust (+4) "Value" (M.fromList [("Value", 40)])
