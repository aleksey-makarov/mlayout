#!/usr/bin/env stack
{- stack script --resolver nightly-2019-05-29
    --package tasty
    --package tasty-hunit
    --package recursion-schemes
-}

{-# LANGUAGE TypeFamilies #-}


{-# OPTIONS_GHC -Wall #-}

import Tree
-- import Data.Functor.Foldable

printTree :: Show a => Tree a -> IO ()
printTree = printTreeOffset 0
  where
    printTreeOffset :: Show a => Word -> Tree a -> IO ()
    printTreeOffset o t = do
      putStrLn $ (map (const ' ') [0 .. o]) ++ (show $ treeData t)
      mapM_ (printTreeOffset (o + 1)) (treeSubtrees t)

myTree :: Tree Word
myTree =
  tree 0 [tree 1 [], tree 2 [tree 3 [], tree 4 []], tree 5 []]

main :: IO ()
main = do
  printTree myTree

