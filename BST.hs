module BST where

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Foldable Tree where  foldMap :: Monoid m => (a -> m) -> Tree a -> m
