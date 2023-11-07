module BinaryTree where

import Test.HUnit

data (Ord a) => BinaryTree a t = Null | Node (a, t) (BinaryTree a t) (BinaryTree a t) deriving (Show)

-- derive equality
instance (Ord a, Eq a, Eq t) => Eq (BinaryTree a t) where
  Null == Null = True
  Node (a, t) l r == Node (a', t') l' r' = a == a' && t == t' && l == l' && r == r'

find :: (Ord a) => BinaryTree a t -> a -> Maybe t
find Null _ = Nothing
find (Node (a, t) l r) a'
  | a < a' = find r a'
  | a > a' = find l a'
  | otherwise = Just t

insert :: (Ord a) => BinaryTree a t -> (a, t) -> BinaryTree a t
insert Null v = Node v Null Null
insert (Node (a, t) l r) (a', t')
  | a < a' = Node (a, t) l (insert r (a', t'))
  | a > a' = Node (a, t) (insert l (a', t')) r
  | otherwise = Node (a, t') l r -- replaces the value t

fromList :: (Ord a) => [(a, t)] -> BinaryTree a t
fromList = foldl insert Null

leftMost Null = Null
leftMost (Node (a, t) Null r) = Node (a, t) Null r
leftMost (Node _ l _) = leftMost l

rightMost Null = Null
rightMost (Node (a, t) l Null) = Node (a, t) l Null
rightMost (Node _ _ r) = rightMost r

inorderSucc :: (Ord a) => BinaryTree a t -> BinaryTree a t
inorderSucc Null = Null
inorderSucc (Node (a, t) l r) = leftMost r

inorderPred :: (Ord a) => BinaryTree a t -> BinaryTree a t
inorderPred Null = Null
inorderPred (Node (a, t) l r) = rightMost l

remove :: (Ord a) => BinaryTree a t -> a -> BinaryTree a t
-- leaf
remove (Node (a, _) Null Null) a' | a == a' = Null
-- one child
remove (Node (a, _) (Node (b, t) l r) Null) a' | a == a' = Node (b, t) l r
remove (Node (a, _) Null (Node (b, t) l r)) a' | a == a' = Node (b, t) l r
-- two children: replace the current node with inorder successor & delete inorder succesor
remove (Node (a, t) l r) a'
  | a < a' = Node (a, t) l (remove r a')
  | a > a' = Node (a, t) (remove l a') r
  | otherwise =
      let (Node (sa, st) sl sr) = inorderSucc (Node (a, t) l r)
       in Node (sa, st) l (remove r sa)

subTree :: (Ord a) => BinaryTree a t -> (a, a) -> BinaryTree a t
subTree Null _ = Null
subTree (Node (a, t) l r) (lo, hi)
  | hi < a = subTree l (lo, hi)
  | a < lo = subTree r (lo, hi)
  | lo <= a && a <= hi = Node (a, t) (subTree l (lo, hi)) (subTree r (lo, hi))
  | otherwise = error "Range error" -- TODO: enforce at the type level

preorderMap :: (Ord a) => BinaryTree a t -> (Maybe t -> b) -> [b]
preorderMap Null f = [f Nothing]
preorderMap (Node (a, t) l r) f = concat [[f (Just t)], preorderMap l f, preorderMap r f]

inorderMap :: (Ord a) => BinaryTree a t -> (Maybe t -> b) -> [b]
inorderMap Null f = [f Nothing]
inorderMap (Node (a, t) l r) f = concat [inorderMap l f, [f (Just t)], inorderMap r f]

postorderMap :: (Ord a) => BinaryTree a t -> (Maybe t -> b) -> [b]
postorderMap Null f = [f Nothing]
postorderMap (Node (a, t) l r) f = concat [postorderMap l f, postorderMap r f, [f (Just t)]]

testInsert :: Test
testInsert = TestCase $ do
  let v = (3 :: Int, 5 :: Int)
  assertEqual "Insertion" (Node v Null Null) (insert Null v)

testFromList :: Test
testFromList = TestCase $ do
  let t = fromList [] :: BinaryTree Int Int
  assertEqual "From List" t Null

  let t = fromList [(1, 1)] :: BinaryTree Int Int
  let r = Node (1, 1) Null Null :: BinaryTree Int Int
  assertEqual "From List" t r

  let t = fromList [(1, 1), (2, 2)] :: BinaryTree Int Int
  let r = Node (1, 1) Null (Node (2, 2) Null Null) :: BinaryTree Int Int
  assertEqual "From List" t r

  let t = fromList [(2, 2), (1, 1), (3, 3)] :: BinaryTree Int Int
  let r = Node (2, 2) (Node (1, 1) Null Null) (Node (3, 3) Null Null) :: BinaryTree Int Int
  assertEqual "From List" t r


testFind :: Test
testFind = TestCase $ do
  let t = Node (100, 2) (Node (99, 1) Null Null) (Node (101, 3) Null Null) :: BinaryTree Int Int
  assertEqual "Find" (Just 3) (find t 101)

tests :: Test
tests = TestList [testInsert, testFromList, testFind]
