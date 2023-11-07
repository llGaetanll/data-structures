module AvlTree where

import Numeric.Natural (Natural)
import Test.HUnit

data AvlTree a t = Null | Node (a, t) (AvlTree a t) (AvlTree a t) deriving (Show)

data Balance = Balanced | LeftHeavy | RightHeavy | LeftLeftHeavy | LeftRightHeavy | RightLeftHeavy | RightRightHeavy

find :: (Ord a) => AvlTree a t -> a -> Maybe t
find Null _ = Nothing
find (Node (a, t) l r) a'
  | a < a' = find r a'
  | a > a' = find l a'
  | otherwise = Just t

rotateLeft :: (Ord a) => AvlTree a t -> AvlTree a t
rotateLeft (Node y (Node x a b) (Node z c d)) = Node z (Node y (Node x a b) c) d

rotateRight :: (Ord a) => AvlTree a t -> AvlTree a t
rotateRight (Node y (Node x a b) (Node z c d)) = Node x a (Node y b (Node z c d))

rotateLeftRight :: (Ord a) => AvlTree a t -> AvlTree a t
rotateLeftRight (Node n l r) = rotateRight (Node n (rotateLeft l) r)

rotateRightLeft :: (Ord a) => AvlTree a t -> AvlTree a t
rotateRightLeft (Node n l r) = rotateLeft (Node n (rotateRight l) r)

height :: (Ord a) => AvlTree a t -> Natural
height Null = 0
height (Node v l r) = 1 + max (height l) (height r)

-- Insert a node into an AVL Tree
insert :: (Ord a) => AvlTree a t -> (a, t) -> AvlTree a t
insert n v = let (t, _) = insert' n v in t
  where
    insert' :: (Ord a) => AvlTree a t -> (a, t) -> (AvlTree a t, Bool)
    insert' Null v = (Node v Null Null, False)
    insert' (Node (a, t) l r) (a', t')
      | a < a' =
          let (r', b) = insert' r (a', t')
              n = Node (a, t) l r'
           in if b then (n, b) else balance n
      | a > a' =
          let (l', b) = insert' l (a', t')
              n = Node (a, t) l' r
           in if b then (n, b) else balance n
      | otherwise = (Node (a, t') l r, False)

-- Balances an AVL Tree
balance :: (Ord a) => AvlTree a t -> (AvlTree a t, Bool)
balance t = case getDeepBalance t of
  LeftLeftHeavy -> (rotateLeft t, True)
  LeftRightHeavy -> (rotateLeftRight t, True)
  RightLeftHeavy -> (rotateRightLeft t, True)
  RightRightHeavy -> (rotateRight t, True)
  _ -> (t, False)

getBalance :: (Ord a) => AvlTree a t -> Balance
getBalance (Node _ l r)
  | d == 0 = Balanced
  | d > 1 = LeftHeavy
  | d < -1 = RightHeavy
  where
    d = fromIntegral (height l) - fromIntegral (height r)

getDeepBalance :: (Ord a) => AvlTree a t -> Balance
getDeepBalance t =
  case getBalance t of
    Balanced -> Balanced
    LeftHeavy ->
      let (Node _ l _) = t
       in case getBalance l of
            Balanced -> LeftHeavy
            LeftHeavy -> LeftLeftHeavy
            RightHeavy -> LeftRightHeavy
    RightHeavy ->
      let (Node _ _ r) = t
       in case getBalance r of
            Balanced -> RightHeavy
            LeftHeavy -> RightLeftHeavy
            RightHeavy -> RightRightHeavy

fromList :: (Ord a) => [(a, t)] -> AvlTree a t
fromList = foldl insert Null

inorderSucc :: (Ord a) => AvlTree a t -> AvlTree a t
inorderSucc Null = Null
inorderSucc (Node (a, t) l r) = leftMost r
  where
    leftMost :: (Ord a) => AvlTree a t -> AvlTree a t
    leftMost Null = Null
    leftMost (Node (a, t) Null r) = Node (a, t) Null r
    leftMost (Node _ l _) = leftMost l

inorderPred :: (Ord a) => AvlTree a t -> AvlTree a t
inorderPred Null = Null
inorderPred (Node (a, t) l r) = rightMost l
  where
    rightMost :: (Ord a) => AvlTree a t -> AvlTree a t
    rightMost Null = Null
    rightMost (Node (a, t) l Null) = Node (a, t) l Null
    rightMost (Node _ _ r) = rightMost r

-- Remove a node from an AVL Tree
remove :: (Ord a) => AvlTree a t -> a -> AvlTree a t
remove n v = let (t, _) = remove' n v in t
  where
  remove' :: (Ord a) => AvlTree a t -> a -> (AvlTree a t, Bool)
  remove' (Node (a, _) Null Null) a' | a == a' = (Null, False)
  remove' (Node (a, _) l Null) a' | a == a' = (l, False)
  remove' (Node (a, _) Null r) a' | a == a' = (r, False)
  remove' (Node (a, t) l r) a'
    | a < a' =
            let (r', b) = remove' r a'
                n = Node (a, t) l r'
             in if b then (n, b) else balance n
    | a > a' =
            let (l', b) = remove' r a'
                n = Node (a, t) l' r
             in if b then (n, b) else balance n
    | otherwise =
        let (Node (a', t') _ _) = inorderSucc (Node (a, t) l r)
            (r', b) = remove' r a'
            n = Node (a', t') l r'
         in if b then (n, b) else balance n

subTree :: (Ord a) => AvlTree a t -> (a, a) -> AvlTree a t
subTree Null _ = Null
subTree (Node (a, t) l r) (lo, hi)
  | hi < a = subTree l (lo, hi)
  | a < lo = subTree r (lo, hi)
  | lo <= a && a <= hi = Node (a, t) (subTree l (lo, hi)) (subTree r (lo, hi))
  | otherwise = error "Range error" -- TODO: enforce at the type level

preorderMap :: (Ord a) => AvlTree a t -> (Maybe t -> b) -> [b]
preorderMap Null f = [f Nothing]
preorderMap (Node (a, t) l r) f = concat [[f (Just t)], preorderMap l f, preorderMap r f]

inorderMap :: (Ord a) => AvlTree a t -> (Maybe t -> b) -> [b]
inorderMap Null f = [f Nothing]
inorderMap (Node (a, t) l r) f = concat [inorderMap l f, [f (Just t)], inorderMap r f]

postorderMap :: (Ord a) => AvlTree a t -> (Maybe t -> b) -> [b]
postorderMap Null f = [f Nothing]
postorderMap (Node (a, t) l r) f = concat [postorderMap l f, postorderMap r f, [f (Just t)]]
