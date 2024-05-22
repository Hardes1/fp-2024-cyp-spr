module BST where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node le val ri) = foldMap f le <> (f val <> foldMap f ri)

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f init Leaf = init
  foldr f init (Node le val ri) = foldr f (f val (foldr f init ri)) le


inOrderTraverse :: Tree a -> [a]
inOrderTraverse tree = doTraverse tree (\val le ri -> le ++ val : ri)

postOrderTraverse :: Tree a -> [a]
postOrderTraverse tree = doTraverse tree (\val le ri -> le ++ ri ++ [val])

preOrderTraverse :: Tree a -> [a]
preOrderTraverse tree = doTraverse tree (\val le ri -> [val] ++ le ++ ri)

doTraverse :: Tree a -> (a -> [a] -> [a] -> [a]) -> [a]
doTraverse Leaf _ = []
doTraverse (Node le val ri) f =
    let leRes = doTraverse le f 
        riRes = doTraverse ri f
        in f val leRes riRes





-- Traverse functions only differs in the sequences of concatenation (I hope that it is allowed to implement this task in such way). 
-- I think foldMap is redundant, but I implemented it