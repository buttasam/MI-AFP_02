module BinarySearchTree where

import qualified Data.List
-- You might want to use some externals. Better use qualified import
-- so there won't be any clash
-- for example instead of "sort" (from Data.List) use "Data.List.sort"

-- !! DO NOT CHANGE BSTree data type and type signatures of functions

-- | Binary search tree as described at wikipedia:
--  https://en.wikipedia.org/wiki/Binary_search_tree
data BSTree a = Node a (BSTree a) (BSTree a)
              | Nil
              deriving (Show, Read, Eq)

value :: BSTree a -> a
value Nil = error "Nil does not have a value"
value (Node x _ _) = x

left :: BSTree a -> BSTree a
left Nil = Nil
left (Node _ l _) = l

right :: BSTree a -> BSTree a
right Nil = Nil
right (Node _ _ r) = r

-- | Check whether is @BSTree@ valid (i.e., does not violate any rule)
isValid :: Ord a => BSTree a -> Bool
isValid Nil = True
isValid tree = sorted (toList tree)

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False

-- | Check whether is @BSTree@ is leaf
isLeaf :: Ord a => BSTree a -> Bool
isLeaf tree = (tree /= Nil) && (left tree == Nil) && (right tree == Nil)

-- | Count all nodes in @BSTree@
size :: BSTree a -> Integer
size Nil = 0
size tree = 1 + (size (left tree)) + (size (right tree))

-- | Height of @BSTree@ (height of @Nil@ is 0)
height :: BSTree a -> Integer
height Nil = 0
height (Node root left right) = (if (height right) > (height left) then (height right) else (height left)) + 1

-- | Minimal height in the @BSTree@ (height of @Nil@ is 0)
minHeight :: BSTree a -> Integer
minHeight Nil = 0
minHeight (Node root left right) = (if (minHeight right) < (minHeight left) then (minHeight right) else (minHeight left)) + 1

-- | Check if given element is in the @BSTree@
contains :: Ord a => BSTree a -> a -> Bool
contains Nil _ = False
contains (Node root left right) a
    | root == a = True
    | a < root = contains left a
    | a > root = contains right a

-- | Create new tree with given element inserted
insert :: Ord a => BSTree a -> a -> BSTree a
insert Nil x = Node x Nil Nil
insert (Node root left right) x
    | root == x = Node root left right
    | root  < x = Node root left (insert right x)
    | root  > x = Node root (insert left x) right

-- | Create new tree with given element deleted (min element in the right subtree strategy)
delete :: Ord a => BSTree a -> a -> BSTree a
delete Nil _ = Nil
delete (Node root left right) x
     | x == root = deleteRoot (Node root left right)
     | x < root = Node root (delete left x) right
     | x > root = Node root left (delete right x)
     where
        deleteRoot :: Ord a => BSTree a -> BSTree a
        deleteRoot (Node root Nil right) = right
        deleteRoot (Node root left Nil) = left
        deleteRoot (Node root left right) = (Node (leftest right) left (delete right (leftest right)))
                                           where
                                              leftest :: (Ord a) => BSTree a -> a
                                              leftest (Node root Nil  _) = root
                                              leftest (Node _ left _) = leftest left

-- | Convert @BSTree@ to list (will be in ascending order if tree is valid)
toList :: BSTree a -> [a]
toList Nil = []
toList tree = (toList (left tree)) ++ [value tree] ++ (toList (right tree))

-- | Build new @BSTree@ from arbitrary list with use of median (left if even)
-- TODO: implement conversion from list to tree, use median (hint: sort)
fromList :: Ord a => [a] -> BSTree a
fromList list = Nil

leftList :: Ord a => [a] -> a -> [a]
leftList (x:xs) a
          | a == x = []
          | otherwise = [x] ++ (leftList xs a)

firstIndex :: Ord a => [a] -> a -> Int
firstIndex (x:xs) a
          | a == x = 0
          | otherwise = 1 + (firstIndex xs a)

rightList :: Ord a => [a] -> a -> [a]
rightList list a = dropWhile (<=a) list


median :: Ord a => [a] -> a
median list = list !! ((div (size + 1) 2) - 1)
     where size = length list