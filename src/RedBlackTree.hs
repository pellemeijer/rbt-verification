{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module RedBlackTree
  ( Color (..),
    nodeT,
    isMember,
    insert,
    listToTree,
    getColor,
  )
where

import GHC.RTS.Flags (DebugFlags (weak))

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
             -> {v:RBTree a | isRedBlackTree v && size v == 1 + size l + size r && color == getColor v}
@-}
nodeT :: Color -> RBTree a -> a -> RBTree a -> RBTree a
nodeT color left val right = Node color left val right

{-@ listToTree :: (Ord a) => [a] -> v:{RBTree a | isRedBlackTree v} @-}
listToTree :: (Ord a) => [a] -> RBTree a
listToTree = foldl (flip insert) Leaf

{-@ isMember :: (Ord a) => x:a -> t:{RBTree a | isRedBlackTree t} -> Bool  @-}
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

{-@ measure size @-}
size :: RBTree a -> Int
size Leaf = 0
size (Node _ left val right) = 1 + size left + size right

{-@ measure getLeft @-}
getLeft :: RBTree a -> RBTree a
getLeft Leaf = Leaf
getLeft (Node _ left _ _) = left

{-@ measure getRight @-}
getRight :: RBTree a -> RBTree a
getRight Leaf = Leaf
getRight (Node _ _ _ right) = right

{-@ insert :: (Ord a) => x:a -> t:{RBTree a | isRedBlackTree t} -> v:{RBTree a | isRedBlackTree t} @-}
insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x tree = makeBlack $ ins tree
  where
    {-@ makeBlack :: t:{RBTree a | weakIsRedBlackTree t} -> v:{RBTree a | isRedBlackTree v} @-}
    makeBlack :: RBTree a -> RBTree a
    makeBlack (Node _ left val right) = nodeT Black left val right
    makeBlack Leaf = Leaf
    {-@ ins :: t:{RBTree a | isRedBlackTree t} -> v:{RBTree a | weakIsRedBlackTree v} @-}
    ins :: RBTree a -> RBTree a
    ins Leaf = nodeT Red Leaf x Leaf
    ins (Node color left val right)
      | x < val = balanceInsert color (ins left) val right
      | x > val = balanceInsert color left val (ins right)
      | otherwise = nodeT color left val right -- x == val

{-@ balanceInsert :: color:Color
                  -> left:{RBTree a | weakIsRedBlackTree left}
                  -> val:a
                  -> right:{RBTree a | weakIsRedBlackTree right && blackHeight left == blackHeight right && (isRedBlackTree left || isRedBlackTree right)}
                  -> v:{RBTree a | weakIsRedBlackTree v && blackHeight v == (if color == Black then 1 else 0) + blackHeight left }
@-}
balanceInsert :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceInsert Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balanceInsert Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)
balanceInsert Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balanceInsert Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balanceInsert color left val right = Node color left val right

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

{-@ inline weakIsRedBlackTree @-}
weakIsRedBlackTree :: RBTree a -> Bool
weakIsRedBlackTree t =
  (if getColor t == Black then isRedBlackTree t else blackInvariant t)
    && isRedBlackTree (getLeft t)
    && isRedBlackTree (getRight t)
