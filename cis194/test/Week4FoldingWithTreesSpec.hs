module Week4FoldingWithTreesSpec
  ( spec
  ) where

import           Test.Hspec

import           Week4.FoldingWithTrees

spec :: Spec
spec = do
  describe "foldTree" $ do
    it "should generate a balanced binary tree" $ do
      foldTree "ABCDEFGHIJKLMNO" `shouldBe` Node 4 (Node 3 (Node 1 (Node 0 Leaf 'A' Leaf) 'I' (Node 0 Leaf 'H' Leaf)) 'M' (Node 1 (Node 0 Leaf 'B' Leaf) 'L' (Node 0 Leaf 'C' Leaf))) 'O' (Node 3 (Node 1 (Node 0 Leaf 'D' Leaf) 'J' (Node 0 Leaf 'G' Leaf)) 'N' (Node 1 (Node 0 Leaf 'E' Leaf) 'K' (Node 0 Leaf 'F' Leaf)))
