#!/usr/bin/env stack
{- stack script --resolver nightly-2019-07-12
    --package text
-}

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

data MemoryItem l r
    = MemoryItemMemory l Text Text (r MLayoutMemory) -- location, name, doc, children
    | MemoryItemWord   l Text Text (r MLayoutWord)

data WordItem l r
    = WordItemWord   l Text Text (r MLayoutWord)
    | WordItemBits   l Text Text (r MLayoutBits)
    | WordItemValue  Integer Text Text -- value, name, doc

data BitsItem l r
    = BitsItemBits  l Text Text (r MLayoutBits)
    | BitsItemValue Integer Text Text

data MLayoutMemory
data MLayoutWord
data MLayoutBits

data MLayoutF :: * -> (* -> *) -> * -> * where
    MLayoutMemoryF :: [ MemoryItem l r ] -> MLayoutF l r MLayoutMemory
    MLayoutWordF ::   [ WordItem l r ]   -> MLayoutF l r MLayoutWord
    MLayoutBitsF ::   [ BitsItem l r ]   -> MLayoutF l r MLayoutBits

-- HFix :: ((* -> *) -> (* -> *)) -> (* -> *)
newtype HFix h a = HFix { unHFix :: h (HFix h) a }

type MLayout = HFix (MLayoutF Location)
type MLayoutParsed = HFix (MLayoutF LocationParsed)

type f :~> g = forall a . f a -> g a

class HFunctor (h :: (* -> *) -> * -> *) where
    hfmap :: (f :~> g) -> h f :~> h g

instance HFunctor (MLayoutF l) where
    hfmap f (MLayoutMemoryF mis) = MLayoutMemoryF $ fmap (ff f) mis
        where
            ff :: (f :~> g) -> MemoryItem l f -> MemoryItem l g
            ff h (MemoryItemMemory l n d c) = MemoryItemMemory l n d $ h c
            ff h (MemoryItemWord l n d c)   = MemoryItemWord l n d $ h c
    hfmap f (MLayoutWordF wis) = MLayoutWordF $ fmap (ff f) wis
        where
            ff :: (f :~> g) -> WordItem l f -> WordItem l g
            ff h (WordItemWord l n d c)   = WordItemWord l n d $ h c
            ff h (WordItemBits l n d c) = WordItemBits l n d $ h c
            ff _ (WordItemValue v n d) = WordItemValue v n d
    hfmap f (MLayoutBitsF bis) = MLayoutBitsF $ fmap (ff f) bis
        where
            ff :: (f :~> g) -> BitsItem l f -> BitsItem l g
            ff h (BitsItemBits l n d c) = BitsItemBits l n d $ h c
            ff _ (BitsItemValue v n d) = BitsItemValue v n d

main :: IO ()
main = putStrLn "Hello World"
