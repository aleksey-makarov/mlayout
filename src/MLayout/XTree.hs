{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}

module MLayout.XTree where

import Data.Functor.Foldable

data XTreeF l n a = XTreeF [Either l (n, a)] deriving (Functor, Traversable, Foldable)

type XTree l n = Fix (XTreeF l n)

empty :: XTree l n
empty = embed $ XTreeF []

null :: XTree l n -> Bool
null (project -> XTreeF []) = True
null _ = False
