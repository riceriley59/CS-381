module HW2 where 

import HW2types

sizeTree :: Tree -> Int
sizeTree Leaf = 0 
sizeTree (Node _ l r) = sizeTree l + sizeTree r + 1  

height :: Tree -> Int

treeSum :: Tree -> Int 

mergeTrees :: Tree Tree -> Tree 

isBst :: Tree -> Bool

convertBst :: Tree -> Tree

numVE :: Graph -> (Int, Int)

removeLoops :: Graph -> Graph

removeVertex :: Vertex Graph -> Graph
