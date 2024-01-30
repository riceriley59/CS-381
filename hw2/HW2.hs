-- Riley Rice
-- riceri@oregonstate.edu
-- CS381 - Programming Language Fundamentals HW2 
-- 1-29-2024

module HW2 where 

import HW2types

-- This function get's the number of nodes in a binary tree.
-- It works by doing a post-order traversal of the tree and adding 1 for 
-- every iteration which counts every node.
sizeTree :: Tree -> Int
sizeTree Leaf = 0 
sizeTree (Node _ l r) = sizeTree l + sizeTree r + 1  

-- This function gets the height of a binary tree. 
-- It works by doing a traversal of each sub tree adding 1 for every
-- recursive level and then takes the max once it comes back up the tree 
-- counting the ammount of levels.
height :: Tree -> Int
height Leaf = -1
height (Node _ l r) = 1 + max (height l) (height r)  

-- This function sums all the values in a tree and returns that.
-- It works very similiarly to the sizeTree function as it does a 
-- pre-order traversal and adds the sum of each Node's value along the way 
treeSum :: Tree -> Int 
treeSum Leaf = 0
treeSum (Node x l r) = x  + treeSum l + treeSum r

-- This is an operator overload for == when dealing with our Tree data structure.
-- It works by summing up the node values of each tree using our treeSum function and
-- comparing the values to see if the trees both have the same amount of values in there.
instance Eq Tree where 
  Leaf == Leaf = True
  tree1 == tree2 = treeSum tree1 == treeSum tree2

mergeTrees :: Tree -> Tree -> Tree 
mergeTrees Leaf tree2 = tree2
mergeTrees tree1 Leaf = tree1
mergeTrees (Node val left right) (Node val2 left2 right2) = 
  Node val (mergeTrees left (Node val2 Leaf Leaf)) (mergeTrees right (Node val2 Leaf Leaf)) 

-- This function checks a given tree to see if it's a valid binary search tree.
-- It works by using a recursive helper function that checks to make sure the given value 
-- of each node is within the range given and we set the previous values as the upper or 
-- lower bounds.
isBST :: Tree -> Bool
isBST Leaf = True
isBST tree = isBSTHelper tree (minBound :: Int) (maxBound :: Int) where 
  isBSTHelper Leaf _ _ = True
  isBSTHelper (Node val left right) minValue maxValue = 
    val > minValue
      && val < maxValue
      && isBSTHelper left minValue val 
      && isBSTHelper right val maxValue

-- This function converts a given tree into a valid binary search tree.
-- It works by using a few different helper functions. First it takes all the values 
-- from a tree and puts it into to a list then uses a recursive function to either 
-- insert the node on the left or right value depending on whether the current Node 
-- is larger or bigger, creating a valid binary search tree.
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

-- This function counts the number of vertices and edges on a given Graph.
-- We first go through all the vertices and then look for unique vertices and 
-- compile the unique vertices into a list. Once we have compiled that list we have
-- the answers we need. The number of vertices will be equal to the list we just made and 
-- the edges will be equal to the lenght of the graph's list of vertices (non-unique).
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

-- This function removes loops from a graph. This is pretty easy as we just need to 
-- look through the list of vertices and look for entries with the same values in each 
-- entry of the tuple. I did this using the filter function to make things more concise.
removeLoops :: Graph -> Graph
removeLoops = filter (\(u, v) -> u /= v)

-- This function removes a given vertex from a graph. It uses the filter function 
-- and checks to see whether either value in the tuple are equal to the given vertex. 
-- If so that entry will be filtered out effectively removing the vertex from the graph.
removeVertex :: Vertex -> Graph -> Graph
removeVertex v = filter (\(u, w) -> u /= v && w /= v)
