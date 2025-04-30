{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module RedBlackTree
  ( Color (..),
    nodeT,
    isMember,
    insert,
    listToTree,
    getColor,
    getVal,
  )
where

data Color = Red | Black
  deriving (Show, Eq)

-- private constructor
data RBTree a = Leaf | Node Color (RBTree a) a (RBTree a)
  deriving (Show, Eq)

-- public smart constructor
{-@ nodeT :: color:Color
             -> {l:RBTree a | isRedBlackTree l && (color == Black || getColor l == Black)}
             -> v:a
             -> {r:RBTree a | isRedBlackTree r && blackHeight r == blackHeight l && (color == Black || getColor r == Black)}
             -> {v:RBTree a | isRedBlackTree v}
@-}
nodeT :: Color -> RBTree a -> a -> RBTree a -> RBTree a
nodeT color left val right = Node color left val right

{-@ listToTree :: (Ord a) => [a] -> v:{RBTree a | isRedBlackTree v} @-}
listToTree :: (Ord a) => [a] -> RBTree a
listToTree = foldl (flip insert) Leaf

{-@ isMember :: (Ord a) => a -> t:{RBTree a | isRedBlackTree t} -> Bool @-}
isMember :: (Ord a) => a -> RBTree a -> Bool
isMember _ Leaf = False
isMember x (Node _ left val right)
  | x < val = isMember x left
  | x > val = isMember x right
  | otherwise = True -- x == val

{-@ measure getColor @-}
getColor :: RBTree a -> Color
getColor (Node color _ _ _) = color
getColor Leaf = Black

{-@ measure getVal @-}
getVal :: RBTree a -> Maybe a
getVal (Node _ _ val _) = Just val
getVal Leaf = Nothing

{-@ insert :: (Ord a) => a -> x:{RBTree a | isRedBlackTree x} -> v:{RBTree a | isRedBlackTree x} @-}
insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x tree = makeBlack $ ins tree
  where
    ins :: RBTree a -> RBTree a
    ins Leaf = nodeT Red Leaf x Leaf
    ins (Node color left val right)
      | x < val = balanceInsert (nodeT color (ins left) val right)
      | x > val = balanceInsert (nodeT color left val (ins right))
      | otherwise = Node color left val right -- x == val
    makeBlack :: RBTree a -> RBTree a
    makeBlack (Node _ left val right) = nodeT Black left val right
    makeBlack Leaf = Leaf

{-@ balanceInsert :: RBTree a -> v:{RBTree a | isRedBlackTree v} @-}
balanceInsert :: RBTree a -> RBTree a
balanceInsert (Node Black (Node Red (Node Red a x b) y c) z d) =
  nodeT Red (nodeT Black a x b) y (nodeT Black c z d)
balanceInsert (Node Black (Node Red a x (Node Red b y c)) z d) =
  nodeT Red (nodeT Black a x b) y (nodeT Black c z d)
balanceInsert (Node Black a x (Node Red (Node Red b y c) z d)) =
  nodeT Red (nodeT Black a x b) y (nodeT Black c z d)
balanceInsert (Node Black a x (Node Red b y (Node Red c z d))) =
  nodeT Red (nodeT Black a x b) y (nodeT Black c z d)
balanceInsert tree = tree

{-@ measure redInvariant @-}
redInvariant :: RBTree a -> Bool
redInvariant t = case t of
  Leaf -> True
  Node color left _ right ->
    (if color == Red then (getColor left == Black && getColor right == Black) else True)
      && (redInvariant left)
      && (redInvariant right)

{-@ measure blackHeight @-}
blackHeight :: RBTree a -> Int
blackHeight Leaf = 1
blackHeight (Node color left _ _) = (if color == Black then 1 else 0) + blackHeight left

{-@ measure blackInvariant @-}
blackInvariant :: RBTree a -> Bool
blackInvariant Leaf = True
blackInvariant (Node color left _ right) =
  blackInvariant left
    && blackInvariant right
    && blackHeight left == blackHeight right

{-@ inline isRedBlackTree @-}
isRedBlackTree :: RBTree a -> Bool
isRedBlackTree t = redInvariant t && blackInvariant t

-- badTree :: RBTree Int
-- badTree = Node Red (Node Red (Node Red Leaf 4 Leaf) 2 Leaf) 1 (Node Black Leaf 3 Leaf)
