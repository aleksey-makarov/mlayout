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

data ValueItem = ValueItem Integer Text Text deriving Show   -- = value, name, doc

data Item a
    = Item
        { _location :: Location
        , _name  :: Text
        , _doc   :: Text
        , _subitem :: a
        } deriving (Show, Functor)

data MemoryItem r
    = MemoryItemMemory (Item (r MLayoutMemory))
    | MemoryItemWord   (Item (r MLayoutWord))

data WordItem r
    = WordItemWord   (Item (r MLayoutWord))
    | WordItemBits   (Item (r MLayoutBits))
    | WordItemValue  ValueItem

data BitsItem r
    = BitsItemBits  (Item (r MLayoutBits))
    | BitsItemValue ValueItem

data MLayoutMemory
data MLayoutWord
data MLayoutBits

data MLayoutF :: (* -> *) -> * -> * where
    MLayoutMemoryF :: [ MemoryItem r ] -> MLayoutF r MLayoutMemory
    MLayoutWordF ::   [ WordItem r ]   -> MLayoutF r MLayoutWord
    MLayoutBitsF ::   [ BitsItem r ]   -> MLayoutF r MLayoutBits

-- HFix :: ((* -> *) -> (* -> *)) -> (* -> *)
newtype HFix h a = HFix { unHFix :: h (HFix h) a }

type MLayout = HFix MLayoutF

type f :~> g = forall a . f a -> g a

class HFunctor (h :: (* -> *) -> * -> *) where
    hfmap :: (f :~> g) -> h f :~> h g

instance HFunctor MLayoutF where
    hfmap f (MLayoutMemoryF mis) = MLayoutMemoryF $ fmap (ff f) mis
        where
            ff :: (f :~> g) -> MemoryItem f -> MemoryItem g
            ff h (MemoryItemMemory i) = MemoryItemMemory $ fmap h i
            ff h (MemoryItemWord i)   = MemoryItemWord $ fmap h i
    hfmap f (MLayoutWordF wis) = MLayoutWordF $ fmap (ff f) wis
        where
            ff :: (f :~> g) -> WordItem f -> WordItem g
            ff h (WordItemWord i)   = WordItemWord $ fmap h i
            ff h (WordItemBits i) = WordItemBits $ fmap h i
            ff _ (WordItemValue i) = WordItemValue i
    hfmap f (MLayoutBitsF bis) = MLayoutBitsF $ fmap (ff f) bis
        where
            ff :: (f :~> g) -> BitsItem f -> BitsItem g
            ff h (BitsItemBits i) = BitsItemBits $ fmap h i
            ff _ (BitsItemValue i) = BitsItemValue i

main :: IO ()
main = putStrLn "Hello World"
