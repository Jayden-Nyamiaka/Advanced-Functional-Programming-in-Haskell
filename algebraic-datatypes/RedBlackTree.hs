-- RedBlackTree.hs
module RedBlackTree where
-- This module provides a type-parameterized definition for red-black trees

import Prelude



-- IMPLEMENTING THE RED-BLACK BINARY TREE

-- A color is either red or black.
data Color = Red | Black
  deriving Show

-- A red-black tree is either a leaf or a tree node with a color,
-- two branches, both of which are trees, and a value of type a.
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
  deriving Show

{-  Takes an elem and red-black tree, return True if the elem is in the 
    tree and false otherwise -}
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member e (Node _ lt cur rt)
    | e == cur = True 
    | e < cur = member e lt 
    | otherwise = member e rt 


-- Takes a red-black tree, returns in-order traversal list of all elems
toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ lt cur rt) = (toList lt) ++ cur : (toList rt)


{-  Takes an elem and a red-black tree and returns a balanced red-black 
    tree with the new elem inserted -} 
insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t)
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right)
        | elem < elem' = balance color (ins elem left) elem' right
        | elem > elem' = balance color left elem' (ins elem right)
        | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- the red and black invariants.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red l1 e1 r1) e2 r2) e t =
        Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    balance Black (Node Red l1 e1 (Node Red l2 e2 r2)) e t =
        Node Red (Node Black l1 e1 l2) e2 (Node Black r2 e t)
    balance Black t e (Node Red (Node Red l2 e2 r2) e1 r1) =
        Node Red (Node Black t e l2) e2 (Node Black r2 e1 r1)
    balance Black t e (Node Red l1 e1 (Node Red l2 e2 r2)) =
        Node Red (Node Black t e l1) e1 (Node Black l2 e2 r2)
    balance color l e r = Node color l e r  -- no balancing needed


-- Takes a list of elems, returns a red-black tree 
fromList :: Ord a => [a] -> Tree a
fromList lst = foldr insert Leaf lst


-- Takes a red-black tree, returns min and max depths (distances)
minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ rt _ lt) = 1 + min (minDepth rt) (minDepth lt)
maxDepth :: Tree a -> Int
maxDepth Leaf = 0
maxDepth (Node _ rt _ lt) = 1 + max (maxDepth rt) (maxDepth lt)



-- TESTING THE RED-BLACK BINARY TREE

{-  Takes a red-black tree, tests the order invariant
    Order invariant: values in left subtree are strictly smaller while 
    values in right subtree are strictly greater for all subtrees -}
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 t = testOrder t Nothing Nothing
    where 
        testOrder :: Ord a => Tree a -> Maybe a -> Maybe a -> Bool
        testOrder Leaf _ _ = True
        testOrder (Node _ lt e rt) min max = isBound e min max &&
            testOrder lt min (Just e) && testOrder rt (Just e) max

        isBound :: Ord a => a -> Maybe a -> Maybe a -> Bool
        isBound _ Nothing Nothing = True
        isBound v (Just sm) Nothing = v > sm
        isBound v Nothing (Just lg) = v < lg
        isBound v (Just sm) (Just lg) = v > sm && v < lg


{-  Takes a red-black tree, tests the red invariant
    Red invariant: no red node has a red parent -}
testInvariant2 :: Tree a -> Bool
testInvariant2 Leaf = True
testInvariant2 (Node Red (Node Red _ _ _) _ _) = False
testInvariant2 (Node Red _ _ (Node Red _ _ _)) = False
testInvariant2 (Node _ lt _ rt) = 
    (testInvariant2 lt) && (testInvariant2 rt)


{-  Takes a red-black tree, tests the black invariant
    Black invariant: all paths from the root to leaves have the same 
    number of black nodes -}
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black lt _ rt) n = 
        (leafCounts lt (n+1)) ++ (leafCounts rt (n+1))
    leafCounts (Node Red lt _ rt) n = 
        (leafCounts lt n) ++ (leafCounts rt n)

    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False



-- UTILIZING THE RED-BLACK BINARY TREE IMPLEMENTATION TO IMPLEMENT SETS

-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set.
toSet :: Ord a => [a] -> Set a
toSet = fromList

-- Takes two sets, return True if first is subset of second
isSubset :: Ord a => Set a -> Set a -> Bool
isSubset sub par = all (\e -> member e par) (toList sub)

-- Takes two sets, returns True if they have the same elems (equality)
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet s1 s2 = isSubset s1 s2 && isSubset s2 s1 

-- Takes two sets, returns a new set that is the union of them
union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = foldr insert s1 (toList s2)

-- Takes two sets, returns a new set that is the intersection of them
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = 
    foldr (\e s3 -> if member e s1 then insert e s3 else s3) empty (toList s2)

-- Takes two sets, returns a new set that is the difference of them
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = 
    foldr (\e s3 -> if member e s2 then s3 else insert e s3) empty (toList s1)
