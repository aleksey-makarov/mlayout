-- https://wiki.haskell.org/GADTs_for_dummies
-- https://en.wikipedia.org/wiki/Generalized_algebraic_data_type
-- https://en.wikibooks.org/wiki/Haskell/GADT

-- THIS >>> http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html
-- https://www.reddit.com/r/haskell/comments/16qr6q/fixing_gadts_generic_recursion_schemes_for_gadts/

-- https://github.com/ekmett/recursion-schemes/issues/43
-- http://hackage.haskell.org/package/compdata-0.12/docs/Data-Comp-Multi-HFunctor.html

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall #-}

module MLayout.Data where

import Data.Text
import Data.List.NonEmpty

data LocationParsed
    = FromTo (Maybe Word) (Maybe Word)                     -- [a:b], a is the first, b is the maximum, not upper bound
    | WordNext Word                                        -- [%32@], [%32], [5@], [5] NB: [2] means [2@], BUT <2> means <@2>
    | Word (Maybe Word) Word                               -- [@0x11], [7@0x22], [%16@3]
    | Fields (Maybe Word) (NonEmpty (Maybe Word, Text))    -- [@{A, B, C}], [%12@{12 A, 34 B}]
    | Periodic (Maybe Word) (Maybe Word) Word (Maybe Word) -- [1@2[3 +4]] means optional width, optional start, mandatory number of items (>= 2), optional step
    deriving Show

data LocationResolved
    = FromToResolved Word Word
    | FieldsResolved Word (NonEmpty (Word, Text))
    | PeriodicResolved Word Word Word Word
    deriving Show

data Location = Location LocationParsed LocationResolved deriving Show

data ValueItem = ValueItem Integer Text Text -- value, name, doc

data MemoryItem r
    = MemoryItemMemory (r MLayoutMemory)
    | MemoryItemWord (r MLayoutWord)

data WordItem r
    = WordItemWord (r MLayoutWord)
    | WordItemBits (r MLayoutBits)
    | WordItemValue ValueItem

data BitsItem r
    = BitsItemBits (r MLayoutBits)
    | BitsItemValue ValueItem

data ItemDescription l = ItemDescription l Text Text  -- location, name, doc

data MLayoutMemory
data MLayoutWord
data MLayoutBits

data MLayoutF :: * -> (* -> *) -> * -> * where
    MLayoutMemoryF :: ItemDescription l -> [ MemoryItem r ] -> MLayoutF l r MLayoutMemory
    MLayoutWordF ::   ItemDescription l -> [ WordItem r ]   -> MLayoutF l r MLayoutWord
    MLayoutBitsF ::   ItemDescription l -> [ BitsItem r ]   -> MLayoutF l r MLayoutBits

-- HFix :: ((* -> *) -> (* -> *)) -> (* -> *)
newtype HFix h a = HFix { unHFix :: h (HFix h) a }

type MLayout = HFix (MLayoutF Location) MLayoutMemory
type MLayoutParsed = HFix (MLayoutF LocationParsed) MLayoutMemory
type WLayoutParsed = HFix (MLayoutF LocationParsed) MLayoutWord
type BLayoutParsed = HFix (MLayoutF LocationParsed) MLayoutBits

type f :~> g = forall a . f a -> g a

class HFunctor (h :: (* -> *) -> * -> *) where
    hfmap :: (f :~> g) -> h f :~> h g

-- FIXME: boilerplate
instance HFunctor (MLayoutF l) where
    hfmap f (MLayoutMemoryF d mis) = MLayoutMemoryF d $ fmap (ff f) mis
        where
            ff :: (f :~> g) -> MemoryItem f -> MemoryItem g
            ff h (MemoryItemMemory x) = MemoryItemMemory $ h x
            ff h (MemoryItemWord x)   = MemoryItemWord $ h x
    hfmap f (MLayoutWordF d wis) = MLayoutWordF d $ fmap (ff f) wis
        where
            ff :: (f :~> g) -> WordItem f -> WordItem g
            ff h (WordItemWord x)   = WordItemWord $ h x
            ff h (WordItemBits x) = WordItemBits $ h x
            -- FIXME: coerce?
            ff _ (WordItemValue vi) = WordItemValue vi
    hfmap f (MLayoutBitsF d bis) = MLayoutBitsF d $ fmap (ff f) bis
        where
            ff :: (f :~> g) -> BitsItem f -> BitsItem g
            ff h (BitsItemBits x) = BitsItemBits $ h x
            ff _ (BitsItemValue vi) = BitsItemValue vi
