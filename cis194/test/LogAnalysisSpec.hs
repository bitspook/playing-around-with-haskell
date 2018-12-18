module LogAnalysisSpec
  ( spec
  ) where

import           Test.Hspec

import           LogAnalysis
import           LogAnalysis.Internal

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should parse a string to message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
