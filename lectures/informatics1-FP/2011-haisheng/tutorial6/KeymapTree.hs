{-# LANGUAGE GADTs #-}

-- INF 1 Functional Programming
--
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT
                  )

where

-- Modules for testing

import Test.QuickCheck
import Data.List

-- The data type
--
{-
-- following style has been already deprecated in Haskell.
data Ord k => Keymap k a = Leaf
                         | Node k a (Keymap k a) (Keymap k a)
                           deriving (Show)
-}

data Keymap k a where
  Leaf :: Keymap k a
  Node :: Ord k => k -> a -> Keymap k a -> Keymap k a -> Keymap k a

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + max (depth left) (depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a l r) = toList l ++ [(k, a)] ++ toList r

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (set key value left) right
                              | otherwise = Node k v left (set key value right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get _ Leaf = Nothing
get key (Node k v l r)
  | key == k = Just v
  | key < k = get key l
  | otherwise = get key r

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr insert Leaf
           where insert (k, a) = set k a


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT key cs = fromList [ a | a@(b, p) <- toList cs, b < key ]

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT key cs = fromList [ a | a@(b, p) <- toList cs, b > key ]

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge x y = fromList $ toList x ++ toList y

prop_merge:: [Int] -> [Int] -> Bool
prop_merge xs ys = toList (fromList zs `merge` fromList zs') == sort (zs ++ zs')
    where zs = zip (nub xs \\ ys) (ys \\ xs)
          zs' = zip (nub ys \\ xs)  (xs \\ ys)

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del key Leaf = Leaf
del key (Node k v l r)
  | key == k = l `merge` r
  | key < k = Node k v (del key l) r
  | otherwise = Node k v l (del k r)

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select p = fromList . filter (\(_,v) -> p v) . toList
