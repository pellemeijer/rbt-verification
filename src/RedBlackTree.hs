{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module RedBlackTree
  ( Color (..),
    RBTree (..),
    isMember,
    okasakiInsert,
    listToTree,
  )
where

data Color = Red | Black
  deriving (Show, Eq)

data RBTree a = Leaf | Node Color (RBTree a) a (RBTree a)
  deriving (Show, Eq)

listToTree :: (Ord a) => [a] -> RBTree a
listToTree = foldl (flip okasakiInsert) Leaf

isMember :: (Ord a) => a -> RBTree a -> Bool
isMember _ Leaf = False
isMember x (Node _ left val right)
  | x < val = isMember x left
  | x == val = True
  | x > val = isMember x right
  | otherwise = error "Error: unreachable case in isMember"

okasakiInsert :: (Ord a) => a -> RBTree a -> RBTree a
okasakiInsert x tree = makeBlack (ins tree)
  where
    ins Leaf = Node Red Leaf x Leaf
    ins (Node color left val right)
      | x < val = balance (Node color (ins left) val right)
      | x == val = Node color left val right
      | x > val = balance (Node color left val (ins right))
      | otherwise = error "Error: unreachable case in okasakiInsert"

makeBlack :: RBTree a -> RBTree a
makeBlack (Node _ left val right) = Node Black left val right
makeBlack Leaf = Leaf

balance :: RBTree a -> RBTree a
balance (Node Black (Node Red (Node Red a x b) y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black (Node Red a x (Node Red b y c)) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red (Node Red b y c) z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red b y (Node Red c z d))) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance tree = tree
