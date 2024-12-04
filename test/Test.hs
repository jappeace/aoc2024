module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Aoc2024.Day2S1

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

oneTwoThree :: [Int]
oneTwoThree = [1, 2, 3]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
       checkReport isAtLeastOneAndAtMostThree [7,6,4,2,1] @?= True
  ]
