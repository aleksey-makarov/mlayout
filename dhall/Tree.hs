{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}

module Tree where

import Data.Functor.Foldable

data TreeF d a = TreeF { _data :: d, _subtrees :: [a] } deriving (Functor, Show)

type Tree d = Fix (TreeF d)

tree :: a -> [Tree a] -> Tree a
tree v l = embed (TreeF v l)

treeData :: Tree a -> a
treeData t = let (TreeF d _) = project t in d

treeSubtrees :: Tree a -> [Tree a]
treeSubtrees t = let (TreeF _ s) = project t in s
