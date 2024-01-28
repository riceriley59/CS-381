module HW2 where 

import HW2types

sizeTree :: Tree -> Int
sizeTree Leaf = 0 
sizeTree (Node _ l r) = sizeTree l + sizeTree r + 1  

height :: Tree -> Int
height Leaf = -1
height (Node _ l r) = 1 + max (height l) (height r)  

treeSum :: Tree -> Int 
treeSum Leaf = 0
treeSum (Node x l r) = x  + treeSum l + treeSum r

--instance Eq Tree where 
--  _ == _ = False
--  Leaf == Leaf = True
--  (Node val left right) == (Node val2 left2 right2) = val == val2 && left == left2 && right == right2
--
--mergeTrees :: Tree Tree -> Tree 
--
--isBst :: Tree -> Bool
--
--convertBst :: Tree -> Tree
--
--numVE :: Graph -> (Int, Int)
--
--removeLoops :: Graph -> Graph
--
--removeVertex :: Vertex Graph -> Graph
