module Week5Spec
  ( spec
  ) where

import qualified Data.Map      as M
import           Test.Hspec
import           Week5.Calc
import           Week5.ExprT
import qualified Week5.StackVM as VM

spec :: Spec
spec = do
  describe "eval" $ do
    it "should return correct result" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalStr" $ do
    it "should return correct result" $ do
      evalStr "2+3*5" `shouldBe` Just 17
      evalStr "2+3*" `shouldBe` Nothing

  describe "Expr" $ do
    it "should create valid instance for integer" $ do
      testInteger `shouldBe` Just (-7)

    it "should create valid instance for Bool" $ do
      testBool `shouldBe` Just True

    it "should create valid instance for MinMax" $ do
      testMM `shouldBe` Just (MinMax 5)

    it "should create valid instance for Mod7" $ do
      testSat `shouldBe` Just (Mod7 5)

  describe "compile" $ do
    it "should evaluate an expression correctly" $ do
      let isCorrect a = case a of
            Just (Right (VM.IVal n)) -> n == 10
            _                        -> False
      isCorrect (VM.stackVM <$> (compile "4 + 3 * 2")) `shouldBe` True

  describe "variables in expressions" $ do
    it "should evaluate variables correctly" $ do
      let
        withVars :: [(String, Integer)]-> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
        withVars vs exp = exp $ M.fromList vs

      (withVars [("x", 6)] $ add (lit 3) (var "x")) `shouldBe` Just 9
      (withVars [("x", 6)] $ add (lit 3) (var "y")) `shouldBe` Nothing
      (withVars [("x", 6), ("y", 3)]$ mul (var "x") (add (var "y") (var "x"))) `shouldBe` Just 54
