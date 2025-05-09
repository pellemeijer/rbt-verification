module Main (main) where

import RedBlackTree
import Test.HUnit

checkRedRed :: RBT a -> Bool
checkRedRed tree
  | isLeaf tree = True
  | getColor tree == Red = not (isRed (getLeft tree)) && not (isRed (getRight tree)) && checkRedRed (getLeft tree) && checkRedRed (getRight tree)
  | getColor tree == Black = checkRedRed (getLeft tree) && checkRedRed (getRight tree)
  | otherwise = error "This case should not happen for a valid RBT"
  where
    isRed t = getColor t == Red

testListToTree :: Test
testListToTree =
  TestLabel "ListToTree Tests" $
    TestList
      [ "listToTree empty list" ~: newLeaf ~=? listToTree ([] :: [Int]),
        "listToTree single element" ~: newTree Black newLeaf 5 newLeaf ~=? listToTree ([5] :: [Int]),
        "listToTree duplicates" ~: listToTree [1, 2, 3] ~=? listToTree ([1, 1, 2, 2, 3] :: [Int]),
        "listToTree ascending order properties"
          ~: let list = ([1 .. 7] :: [Int])
                 tree = listToTree list
              in TestList
                   [ TestCase $ checkRedRed tree @? "No Red-Red (asc)",
                     TestCase $ all (`isMember` tree) list @? "Members present (asc)",
                     TestCase $ not (isMember 0 tree) @? "Non-member absent (asc)"
                   ],
        "listToTree descending order properties"
          ~: let list = ([7, 6 .. 1] :: [Int])
                 tree = listToTree list
              in TestList
                   [ TestCase $ checkRedRed tree @? "No Red-Red (desc)",
                     TestCase $ all (`isMember` tree) list @? "Members present (desc)",
                     TestCase $ not (isMember 8 tree) @? "Non-member absent (desc)"
                   ],
        "listToTree mixed order properties"
          ~: let list = ([3, 1, 4, 5, 9, 2, 6] :: [Int])
                 tree = listToTree list
              in TestList
                   [ TestCase $ checkRedRed tree @? "No Red-Red (mixed)",
                     TestCase $ all (`isMember` tree) list @? "Members present (mixed)",
                     TestCase $ not (isMember 7 tree) @? "Non-member absent (mixed)"
                   ]
      ]

testIsMember :: Test
testIsMember =
  TestLabel "isMember Tests" $
    TestList
      [ "member in empty" ~: False ~=? isMember (5 :: Int) newLeaf,
        "member exists root" ~: True ~=? isMember 10 (listToTree ([10] :: [Int])),
        "member exists left" ~: True ~=? isMember 5 (listToTree ([10, 5] :: [Int])),
        "member exists right" ~: True ~=? isMember 15 (listToTree ([10, 15] :: [Int])),
        "member exists complex" ~: True ~=? isMember 7 (listToTree ([10, 5, 15, 3, 7, 12, 18] :: [Int])),
        "member missing complex" ~: False ~=? isMember 9 (listToTree ([10, 5, 15, 3, 7, 12, 18] :: [Int])),
        "member missing smaller" ~: False ~=? isMember 1 (listToTree ([10, 5, 15] :: [Int])),
        "member missing larger" ~: False ~=? isMember 20 (listToTree ([10, 5, 15] :: [Int]))
      ]

tree1_10 :: RBT Int
tree1_10 = listToTree ([1 .. 10] :: [Int])

-- Helper function to structurally compare non-leaf nodes
structuralEqNode :: (Eq a) => RBT a -> RBT a -> Bool
structuralEqNode t1 t2
  | not (isLeaf t1) && not (isLeaf t2) =
      getColor t1 == getColor t2
        && structuralEq (getLeft t1) (getLeft t2)
        && getVal t1 == getVal t2
        && structuralEq (getRight t1) (getRight t2)
  | otherwise = False

-- Helper function for structural equality using isLeaf and getters
structuralEq :: (Eq a) => RBT a -> RBT a -> Bool
structuralEq t1 t2
  | isLeaf t1 && isLeaf t2 = True
  | not (isLeaf t1) && not (isLeaf t2) = structuralEqNode t1 t2
  | otherwise = False

testInsert :: Test
testInsert =
  TestLabel "insert Tests" $
    TestList
      [ "insert into empty" ~: newTree Black newLeaf 5 newLeaf ~=? insert (5 :: Int) newLeaf,
        "insert duplicate" ~: listToTree ([10, 5] :: [Int]) ~=? insert 5 (listToTree ([10, 5] :: [Int])),
        "insert 1..3 check structure"
          ~: let expected = newTree Black (newTree Black newLeaf 1 newLeaf) 2 (newTree Black newLeaf 3 newLeaf)
              in TestCase $ structuralEq expected (listToTree ([1, 2, 3] :: [Int])) @? "insert 1..3 structure",
        "insert 3..1 check structure"
          ~: let expected = newTree Black (newTree Black newLeaf 1 newLeaf) 2 (newTree Black newLeaf 3 newLeaf)
              in TestCase $ structuralEq expected (listToTree ([3, 2, 1] :: [Int])) @? "insert 3..1 structure",
        "insert 1..10 no red-red" ~: checkRedRed tree1_10 @? "1..10 No Red-Red",
        "insert 1..10 members" ~: all (`isMember` tree1_10) [1 .. 10] @? "1..10 Members"
      ]

main :: IO ()
main = do
  putStrLn "\nRunning RedBlackTree Tests...\n"
  let allTests =
        TestList
          [ testListToTree,
            testIsMember,
            testInsert
          ]
  counts <- runTestTT allTests
  if errors counts + failures counts == 0
    then putStrLn "\nAll RedBlackTree tests passed!"
    else putStrLn $ "\nTests failed: " ++ show (errors counts) ++ " errors, " ++ show (failures counts) ++ " failures."
