module Week4.FoldingWithTrees where

import           Debug.Trace (trace)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ l v r) = case compare dl dr of
  LT -> l'
  GT -> r'
  EQ -> if isBalanced r then l' else r'
  where
    dl = depth l
    dr = depth r
    l' = Node (1 + dl) (insert x l) v r
    r' = Node (1 + dr) l v (insert x r)

depth :: Tree a -> Integer
depth Leaf           = 0
depth (Node _ l _ r) = 1 + max (depth l) (depth r)

isBalanced :: Tree a -> Bool
isBalanced Leaf           = False
isBalanced (Node _ l _ r) =
  if depth l /= depth r then False
  else case (l, r) of
    (Leaf, Leaf) -> True
    (Leaf, _)    -> False
    (_, Leaf)    -> False
    (l', r')     -> isBalanced l' && isBalanced r'

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
