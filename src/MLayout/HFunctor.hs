{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- from Data.Comp.Multi.*

module MLayout.HFunctor where

type f :~> g = forall a . f a -> g a

class HFunctor (h :: (* -> *) -> * -> *) where
    hfmap :: (f :~> g) -> h f :~> h g

-- HFix :: ((* -> *) -> (* -> *)) -> (* -> *)
newtype HFix h a = HFix { unHFix :: h (HFix h) a }

hcata :: forall h a . HFunctor h => (h a :~> a) -> HFix h :~> a
hcata f = run
    where run :: HFix h :~> a
          run (HFix t) = f (hfmap run t)
