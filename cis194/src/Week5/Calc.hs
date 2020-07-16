{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Week5.Calc where

import qualified Data.Map      as M
import           Week5.ExprT
import           Week5.Parser
import qualified Week5.StackVM as VM

eval :: ExprT -> Integer
eval x = case x of
  Lit n   -> n
  Add a b -> (eval a) + (eval b)
  Mul a b -> (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add a b = Add a b
  mul a b = Mul a b

instance Expr Integer where
  lit x = x
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit a
    | a <= 0 = False
    | otherwise = True
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord)

mod7 a
  | a < 0 = 0
  | a < 7 = a
  | otherwise = mod7 (a `mod` 7)

instance Expr Mod7 where
  lit a = Mod7 $ mod7 a
  add (Mod7 a) (Mod7 b) = Mod7 $  mod7 (a + b)
  mul (Mod7 a) (Mod7 b) = Mod7 $  mod7 (a * b)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

---

instance Expr VM.Program where
  lit a = [VM.PushI a]
  add a b = a ++ b ++ [VM.Add]
  mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExperT = VLit Integer
               | VVar String
               | VAdd VarExperT VarExperT
               | VMul VarExperT VarExperT
  deriving (Show, Eq)

instance Expr VarExperT where
  lit a = VLit a
  add a b = VAdd a b
  mul a b = VMul a b

instance HasVars VarExperT where
  var x = VVar x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = \_ -> Just a
  add a b = \map -> (+) <$> (a map) <*> (b map)
  mul a b = \map -> (*) <$> (a map) <*> (b map)
