{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}

module Tree where

import Data.Functor.Foldable

data TreeF d a = TreeF { _data :: d, _subtrees :: [a] } deriving (Functor, Show)
type Tree d = Fix (TreeF d)

x :: IO ()
x = putStrLn "Hello world"
