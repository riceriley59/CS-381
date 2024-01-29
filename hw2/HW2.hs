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

instance Eq Tree where 
  Leaf == Leaf = True
  (Node val left right) == (Node val2 left2 right2) = val == val2 && left == left2 && right == right2
  Leaf == (Node _ _ _) = False
  (Node _ _ _) == Leaf = False

mergeTrees :: Tree -> Tree -> Tree 
mergeTrees Leaf t2 = t2
mergeTrees t1 Leaf = t1
mergeTrees (Node val left right) t2 = Node val (mergeTrees left t2) (mergeTrees right t2)

isBST :: Tree -> Bool
isBST Leaf = True
isBST tree = isBSTHelper tree (minBound :: Int) (maxBound :: Int) where 
  isBSTHelper Leaf _ _ = True
  isBSTHelper (Node val left right) minValue maxValue = 
    val > minValue
      && val < maxValue
      && isBSTHelper left minValue val 
      && isBSTHelper right val maxValue

convertBST :: Tree -> Tree
convertBST tree = fromList (collectValues tree)
  where
    collectValues :: Tree -> [Int]
    collectValues Leaf = []
    collectValues (Node val left right) =
      collectValues left ++ [val] ++ collectValues right

    fromList :: [Int] -> Tree
    fromList = foldr insertBST Leaf

    insertBST :: Int -> Tree -> Tree
    insertBST x Leaf = Node x Leaf Leaf
    insertBST x (Node val left right)
      | x <= val = Node val (insertBST x left) right
      | otherwise = Node val left (insertBST x right)

numVE :: Graph -> (Int, Int)
numVE graph = (numVertices, numEdges) where 
  collectVertices :: Graph -> [Vertex] -> [Vertex]
  collectVertices [] acc = acc
  collectVertices ((u, v):edges) acc = collectVertices edges (collectUnique u (collectUnique v acc))

  collectUnique :: Vertex -> [Vertex] -> [Vertex]
  collectUnique x xs
    | x `elem` xs = xs
    | otherwise = x : xs

  uniqueVertices = collectVertices graph []

  numVertices = length uniqueVertices
  numEdges = length graph

removeLoops :: Graph -> Graph
removeLoops = filter (\(u, v) -> u /= v)

removeVertex :: Vertex -> Graph -> Graph
removeVertex v = filter (\(u, w) -> u /= v && w /= v)
