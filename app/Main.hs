module Main where

import BinaryTree
import AvlTree

import Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT BinaryTree.tests
  if errors counts + failures counts == 0
    then putStrLn "All tests passed!"
    else putStrLn "Some tests failed."
