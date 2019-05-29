#!/usr/bin/env stack
{- stack script --resolver nightly-2019-05-29
    --package tasty
    --package tasty-hunit
    --package recursion-schemes
-}

{-# LANGUAGE TypeFamilies #-}


{-# OPTIONS_GHC -Wall #-}

import Tree
import Data.Functor.Foldable

mkTree :: a -> [Tree a] -> Tree a
mkTree v l = embed (TreeF v l)

printTree :: Show a => Tree a -> IO ()
printTree = printTreeOffset 0
  where
    printTreeOffset :: Show a => Word -> Tree a -> IO ()
    printTreeOffset o t = do
      let (TreeF v l) = project t
      putStrLn $ (map (const ' ') [0 .. o]) ++ (show v)
      _ <- mapM (printTreeOffset (o + 1)) l
      return ()

myTree :: Tree Word
myTree =
  mkTree 0 [mkTree 1 [], mkTree 2 [mkTree 3 [], mkTree 4 []], mkTree 5 []]

main :: IO ()
main = do
  printTree myTree

