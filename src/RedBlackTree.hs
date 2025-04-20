{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module RedBlackTree
  ( Color (..),
    RBTree (..),
    isMember,
    insert,
    listToTree,
  )
where

data Color = Red | Black
  deriving (Show, Eq)

data RBTree a = Leaf | Node Color (RBTree a) a (RBTree a)
  deriving (Show, Eq)

listToTree :: (Ord a) => [a] -> RBTree a
listToTree = foldl (flip insert) Leaf

isMember :: (Ord a) => a -> RBTree a -> Bool
isMember _ Leaf = False
isMember x (Node _ left val right)
  | x < val = isMember x left
  | x > val = isMember x right
  | otherwise = True -- x == val

makeBlack :: RBTree a -> RBTree a
makeBlack (Node _ left val right) = Node Black left val right
makeBlack Leaf = Leaf

insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x tree = makeBlack $ ins tree
  where
    ins Leaf = Node Red Leaf x Leaf
    ins (Node color left val right)
      | x < val = balanceInsert (Node color (ins left) val right)
      | x > val = balanceInsert (Node color left val (ins right))
      | otherwise = Node color left val right -- x == val

balanceInsert :: RBTree a -> RBTree a
balanceInsert (Node Black (Node Red (Node Red a x b) y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balanceInsert (Node Black (Node Red a x (Node Red b y c)) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balanceInsert (Node Black a x (Node Red (Node Red b y c) z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)
balanceInsert (Node Black a x (Node Red b y (Node Red c z d))) =
  Node Red (Node Black a x b) y (Node Black c z d)
balanceInsert tree = tree
