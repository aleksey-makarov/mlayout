{-# LANGUAGE DeriveFunctor #-}

module XTree where

import Data.Functor.Foldable

data XTreeBase l n a = XTreeBase [Either l (n, a)] deriving Functor

type XTree l n = Fix (XTreeBase l n)
