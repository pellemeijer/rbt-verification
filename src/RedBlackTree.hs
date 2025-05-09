{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module RedBlackTree
  ( Color (..),
    newTree,
    isMember,
    insert,
    listToTree,
    getColor,
    getLeft,
    getRight,
    isLeaf,
    getSize,
    getBlackHeight,
    getVal,
    newLeaf,
    RBT,
  )
where

--------------------------------------------------------------------------------
----------------------------- DATA TYPES ---------------------------------------
--------------------------------------------------------------------------------

data Color = Red | Black
  deriving (Show, Eq)

-- private constructor
data RBT a = Leaf | Node Color (RBT a) a (RBT a)
  deriving (Show, Eq)

-- public smart constructor
{-@
newTree :: color:Color
        -> {l:RBT a | isRBT l &&
                      (color == Black || getColor l == Black)}
        -> x:a
        -> {r:RBT a | isRBT r &&
                      getBlackHeight r == getBlackHeight l &&
                      (color == Black || getColor r == Black)}
        -> {v:RBT a | isRBT v &&
                      getSize v == 1 + getSize l + getSize r &&
                      color == getColor v}
@-}
newTree :: Color -> RBT a -> a -> RBT a -> RBT a
newTree color left val right = Node color left val right

{-@ newLeaf :: v:{RBT a | isLeaf v} @-}
newLeaf :: RBT a
newLeaf = Leaf

--------------------------------------------------------------------------------
----------------------------- PUBLIC GETTERS -----------------------------------
--------------------------------------------------------------------------------

{-@ measure getColor @-}
getColor :: RBT a -> Color
getColor (Node color _ _ _) = color
getColor Leaf = Black

{-@ measure getSize @-}
getSize :: RBT a -> Int
getSize Leaf = 0
getSize (Node _ left _ right) = 1 + getSize left + getSize right

{-@ measure getLeft @-}
getLeft :: RBT a -> RBT a
getLeft Leaf = Leaf
getLeft (Node _ left _ _) = left

{-@ measure getRight @-}
getRight :: RBT a -> RBT a
getRight Leaf = Leaf
getRight (Node _ _ _ right) = right

{-@ measure getBlackHeight @-}
getBlackHeight :: RBT a -> Int
getBlackHeight Leaf = 1
getBlackHeight (Node color left _ _) =
  (if color == Black then 1 else 0) + getBlackHeight left

{-@ measure isLeaf @-}
isLeaf :: RBT a -> Bool
isLeaf Leaf = True
isLeaf _ = False

{-@ measure getVal @-}
getVal :: RBT a -> Maybe a
getVal Leaf = Nothing
getVal (Node _ _ val _) = Just val

--------------------------------------------------------------------------------
----------------------------- PUBLIC FUNCTIONS ---------------------------------
--------------------------------------------------------------------------------

{-@ listToTree :: (Ord a) => [a] -> v:{RBT a | isRBT v} @-}
listToTree :: (Ord a) => [a] -> RBT a
listToTree = foldl (flip insert) Leaf

{-@ isMember :: (Ord a) => x:a -> t:{RBT a | isRBT t} -> Bool  @-}
isMember :: (Ord a) => a -> RBT a -> Bool
isMember _ Leaf = False
isMember x (Node _ left val right)
  | x < val = isMember x left
  | x > val = isMember x right
  | otherwise = True -- x == val

{-@
insert :: (Ord a) => x:a
                  -> t:{RBT a | isRBT t}
                  -> v:{RBT a | isRBT v}
@-}
insert :: (Ord a) => a -> RBT a -> RBT a
insert x tree = makeBlack $ ins x tree

--------------------------------------------------------------------------------
----------------------------- PRIVATE HELPERS ----------------------------------
--------------------------------------------------------------------------------

{-@ makeBlack :: t:{RBT a | isWeak t} -> v:{RBT a | isRBT v} @-}
makeBlack :: RBT a -> RBT a
makeBlack (Node _ left val right) = newTree Black left val right
makeBlack Leaf = Leaf

{-@
ins :: (Ord a) => a
               -> t:{RBT a | isRBT t}
               -> v:{RBT a | isWeak v &&
                             (getColor t /= Red => redInvariant v) &&
                             getBlackHeight v == getBlackHeight t}
@-}
ins :: (Ord a) => a -> RBT a -> RBT a
ins newVal Leaf = Node Red Leaf newVal Leaf
ins newVal (Node color left val right)
  | newVal < val = balanceInsertL color (ins newVal left) val right
  | newVal > val = balanceInsertR color left val (ins newVal right)
  | otherwise = Node color left val right -- newVal == val

{-@
balanceInsertL :: (Ord a)
               => color:Color
               -> left:{RBT a | isWeak left &&
                                (color == Red => redInvariant left)}
               -> val:a
               -> right:{RBT a | isRBT right &&
                                 getBlackHeight right == getBlackHeight left}
               -> v:{RBT a | isWeak v &&
                             (color /= Red => redInvariant v) &&
                             getBlackHeight v ==
                               (if color == Black then 1 else 0) +
                               getBlackHeight left}
@-}
balanceInsertL :: (Ord a) => Color -> RBT a -> a -> RBT a -> RBT a
balanceInsertL Black (Node Red (Node Red a x b) y c) z d =
  Node Red (Node Black a x b) y (Node Black c z d)
balanceInsertL Black (Node Red a x (Node Red b y c)) z d =
  Node Red (Node Black a x b) y (Node Black c z d)
balanceInsertL color left val right = Node color left val right

{-@
balanceInsertR :: (Ord a)
               => color:Color
               -> left:{RBT a | isRBT left}
               -> val:a
               -> right:{RBT a | isWeak right &&
                                 (color == Red => redInvariant right) &&
                                 getBlackHeight right == getBlackHeight left}
               -> v:{RBT a | isWeak v &&
                             (color /= Red => redInvariant v) &&
                             getBlackHeight v ==
                               (if color == Black then 1 else 0) +
                               getBlackHeight left}
@-}
balanceInsertR :: (Ord a) => Color -> RBT a -> a -> RBT a -> RBT a
balanceInsertR Black a x (Node Red (Node Red b y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balanceInsertR Black a x (Node Red b y (Node Red c z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)
balanceInsertR color left val right = Node color left val right

--------------------------------------------------------------------------------
----------------------------- PRIVATE INVARIANTS -------------------------------
--------------------------------------------------------------------------------

{-@ measure redInvariant @-}
redInvariant :: RBT a -> Bool
redInvariant Leaf = True
redInvariant (Node color left _ right) =
  ( if color == Red
      then
        (getColor left /= Red && getColor right /= Red)
      else True
  )
    && redInvariant left
    && redInvariant right

{-@ measure blackInvariant @-}
blackInvariant :: RBT a -> Bool
blackInvariant Leaf = True
blackInvariant (Node _ left _ right) =
  getBlackHeight left == getBlackHeight right
    && blackInvariant left
    && blackInvariant right

{-@ inline isRBT @-}
isRBT :: RBT a -> Bool
isRBT t = redInvariant t && blackInvariant t

{-@ inline isWeak @-}
isWeak :: RBT a -> Bool
isWeak t =
  blackInvariant t
    && redInvariant (getLeft t)
    && redInvariant (getRight t)
