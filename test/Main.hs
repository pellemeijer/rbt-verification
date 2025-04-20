module Main (main) where

import RedBlackTree
import Test.HUnit

checkRedRed :: RBTree a -> Bool
checkRedRed Leaf = True
checkRedRed (Node Black left _ right) = checkRedRed left && checkRedRed right
checkRedRed (Node Red left _ right) = checkChild left && checkChild right && checkRedRed left && checkRedRed right
  where
    checkChild (Node color _ _ _)
      | color == Red = False
      | otherwise = True
    checkChild Leaf = True

testListToTree :: Test
testListToTree =
  TestLabel "ListToTree Tests" $
    TestList
      [ "listToTree empty list" ~: Leaf ~=? listToTree ([] :: [Int]),
        "listToTree single element" ~: Node Black Leaf 5 Leaf ~=? listToTree ([5] :: [Int]),
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
      [ "member in empty" ~: False ~=? isMember (5 :: Int) Leaf,
        "member exists root" ~: True ~=? isMember 10 (listToTree ([10] :: [Int])),
        "member exists left" ~: True ~=? isMember 5 (listToTree ([10, 5] :: [Int])),
        "member exists right" ~: True ~=? isMember 15 (listToTree ([10, 15] :: [Int])),
        "member exists complex" ~: True ~=? isMember 7 (listToTree ([10, 5, 15, 3, 7, 12, 18] :: [Int])),
        "member missing complex" ~: False ~=? isMember 9 (listToTree ([10, 5, 15, 3, 7, 12, 18] :: [Int])),
        "member missing smaller" ~: False ~=? isMember 1 (listToTree ([10, 5, 15] :: [Int])),
        "member missing larger" ~: False ~=? isMember 20 (listToTree ([10, 5, 15] :: [Int]))
      ]

tree1_10 :: RBTree Int
tree1_10 = listToTree ([1 .. 10] :: [Int])

testOkasakiInsert :: Test
testOkasakiInsert =
  TestLabel "insert Tests" $
    TestList
      [ "insert into empty" ~: Node Black Leaf 5 Leaf ~=? insert (5 :: Int) Leaf,
        "insert duplicate" ~: listToTree ([10, 5] :: [Int]) ~=? insert 5 (listToTree ([10, 5] :: [Int])),
        "insert 1..3 check structure" ~: Node Black (Node Black Leaf 1 Leaf) 2 (Node Black Leaf 3 Leaf) ~=? listToTree ([1, 2, 3] :: [Int]),
        "insert 3..1 check structure" ~: Node Black (Node Black Leaf 1 Leaf) 2 (Node Black Leaf 3 Leaf) ~=? listToTree ([3, 2, 1] :: [Int]),
        "insert 1..10 no red-red" ~: checkRedRed tree1_10 @? "1..10 No Red-Red",
        "insert 1..10 members" ~: all (`isMember` tree1_10) [1 .. 10] @? "1..10 Members"
      ]

main :: IO ()
main = do
  putStrLn "\nRunning RedBlackTree Tests...\n"
  -- Combine all test sets
  let allTests =
        TestList
          [ testListToTree, -- Add new tests
            testIsMember,
            testOkasakiInsert
            -- , testDelete -- Add when implemented
          ]
  -- Run tests and print results
  counts <- runTestTT allTests
  if errors counts + failures counts == 0
    then putStrLn "\nAll RedBlackTree tests passed!"
    else putStrLn $ "\nTests failed: " ++ show (errors counts) ++ " errors, " ++ show (failures counts) ++ " failures."
