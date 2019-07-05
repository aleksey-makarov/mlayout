{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

{-# OPTIONS_GHC -Wall #-}

module XTree where

import Data.Functor.Foldable

data XTreeF l n a = XTreeF [Either l (n, a)] deriving (Functor, Traversable, Foldable)

type XTree l n = Fix (XTreeF l n)
