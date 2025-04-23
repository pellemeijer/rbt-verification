{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module RedBlackTree
  ( Color (..),
    RBTree (..),
    isMember,
    insert,
    listToTree,
    getColor,
    blackHeight,
    getVal,
  )
where

data Color = Red | Black
  deriving (Show, Eq)

data RBTree a = Leaf | Node Color (RBTree a) a (RBTree a)
  deriving (Show, Eq)

-- {-@
-- data RBTree a where
--   Leaf :: {leaf:RBTree a | blackHeight leaf == 1}
--   Node :: color:Color
--        -> left:{v:RBTree a | redRedInvariant v}
--        -> val:a
--        -> right:{v:RBTree a | redRedInvariant v}
-- @-}

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

{-@ measure getColor @-}
getColor :: RBTree a -> Color
getColor (Node color _ _ _) = color
getColor Leaf = Black

{-@ measure getVal @-}
getVal :: RBTree a -> Maybe a
getVal (Node _ _ val _) = Just val
getVal Leaf = Nothing

{-@ measure getLeft @-}
getLeft :: RBTree a -> RBTree a
getLeft Leaf = Leaf
getLeft (Node _ left _ _) = left

{-@ measure getRight @-}
getRight :: RBTree a -> RBTree a
getRight Leaf = Leaf
getRight (Node _ _ _ right) = right

-- returns black-height assuming a properly colored/balanced tree
{-@ measure blackHeight @-}
blackHeight :: RBTree a -> Int
blackHeight Leaf = 1
blackHeight (Node color left _ _)
  | color == Black = 1 + blackHeight left
  | otherwise = blackHeight left

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

{-@ inline redRedInvariant @-}
redRedInvariant :: RBTree a -> Bool
redRedInvariant tree
  | getColor tree == Red = getColor (getLeft tree) /= Red && getColor (getRight tree) /= Red
  | otherwise = True

{-@ badTree :: {v:RBTree Int | redRedInvariant v} @-}
badTree :: RBTree Int
badTree = Node Red (Node Red (Node Red Leaf 4 Leaf) 2 Leaf) 1 (Node Black Leaf 3 Leaf)
