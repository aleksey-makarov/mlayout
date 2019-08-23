{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- from Data.Comp.Multi.*

module MLayout.HFunctor where

import Control.Applicative
import Data.Monoid

infixr 0 :~>
type f :~> g = forall a . f a -> g a

type NatM m f g = forall i. f i -> m (g i)

infixr 0 :=>
type f :=> a = forall i . f i -> a

class HFunctor (h :: (* -> *) -> * -> *) where
    hfmap :: (f :~> g) -> h f :~> h g

class HFunctor h => HFoldable h where
    hfold :: Monoid m => h (Const m) :=> m
    hfold = hfoldMap getConst

    hfoldMap :: Monoid m => (a :=> m) -> h a :=> m
    hfoldMap f = hfoldr (mappend . f) mempty

    hfoldr :: (a :=> (b -> b)) -> b -> h a :=> b
    hfoldr f z t = appEndo (hfoldMap (Endo . f) t) z

class HFoldable t => HTraversable t where
    htraverse :: (Applicative f) => NatM f a b -> NatM f (t a) (t b)

-- HFix :: ((* -> *) -> (* -> *)) -> (* -> *)
newtype HFix h a = HFix { unHFix :: h (HFix h) a }

hcata :: forall h a . HFunctor h => (h a :~> a) -> HFix h :~> a
hcata f = run
    where run :: HFix h :~> a
          run (HFix t) = f (hfmap run t)

hcataM :: forall h m a . (HTraversable h, Monad m) => NatM m (h a) a -> NatM m (HFix h) a
hcataM alg = run
    where run :: NatM m (HFix h) a
          run (HFix x) = alg =<< htraverse run x
