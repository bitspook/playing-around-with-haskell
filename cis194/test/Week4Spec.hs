module Week4Spec
  ( spec
  ) where

import           Test.Hspec

import           Week4.FoldingWithTrees
import           Week4.MoreFolds
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

  describe "foldTree" $ do
    it "should generate a balanced binary tree" $ do
      foldTree "ABCDEFGHIJKLMNO" `shouldBe` Node 4 (Node 3 (Node 1 (Node 0 Leaf 'A' Leaf) 'I' (Node 0 Leaf 'H' Leaf)) 'M' (Node 1 (Node 0 Leaf 'B' Leaf) 'L' (Node 0 Leaf 'C' Leaf))) 'O' (Node 3 (Node 1 (Node 0 Leaf 'D' Leaf) 'J' (Node 0 Leaf 'G' Leaf)) 'N' (Node 1 (Node 0 Leaf 'E' Leaf) 'K' (Node 0 Leaf 'F' Leaf)))

  describe "xor" $ do
    it "should return true only if there are odd number of True entries" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False

  describe "map'" $ do
    it "should map the function over a list to create new list of mapped values" $ do
      map' (2 *) [1, 2, 3, 4] `shouldBe` [2, 4, 6, 8]
