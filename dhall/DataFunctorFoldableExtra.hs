{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module DataFunctorFoldableExtra where

import Control.Monad
import Data.Functor.Foldable

-- | A monadic catamorphism
cataM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< mapM c <=< (return . project)

-- | A monadic anamorphism
anaM
  :: (Corecursive t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = (return . embed) <=< mapM a <=< g

data TreeF d a = TreeF { rootLabelF :: d, subForestF :: [a] } deriving (Functor, Show)
