module Tree where

data Tree = T String
          | Node Tree Tree

treeG :: [String] -> Integer -> (Tree, [String])
treeG (x:xs) 0 = (T x, xs)
treeG l n = (Node t1 t2, l2)
  where
    (t1, l1) = treeG l  (n-1)
    (t2, l2) = treeG l1 (n-1)
