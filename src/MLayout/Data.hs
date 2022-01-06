-- https://wiki.haskell.org/GADTs_for_dummies
-- https://en.wikipedia.org/wiki/Generalized_algebraic_data_type
-- https://en.wikibooks.org/wiki/Haskell/GADT

-- THIS >>> http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html
-- https://www.reddit.com/r/haskell/comments/16qr6q/fixing_gadts_generic_recursion_schemes_for_gadts/

-- https://github.com/ekmett/recursion-schemes/issues/43
-- http://hackage.haskell.org/package/compdata-0.12/docs/Data-Comp-Multi-HFunctor.html

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

module MLayout.Data where

-- import Control.Applicative
import Data.List.NonEmpty
import Data.Text
import Data.Text.Prettyprint.Doc

-- import MLayout.HFunctor

data WordWidth = W8 | W16 | W32 | W64 | W128 deriving Show

data StartParsed
    = Next                                    -- [%32@], [%32], [5@], [5] NB: [2] means [2@], BUT <2> means <@2>
    | Simple Word                             -- [@0x11], [7@0x22], [%16@3]
    | Fields (NonEmpty (Maybe Word, Text))    -- [@{A, B, C}], [%12@{12 A, 34 B}]
    | Periodic (Maybe Word) Word (Maybe Word) -- [1@2[3 +4]] means optional start, mandatory number of items (>= 2), optional step
    deriving Show

data LocationMB
    = FromTo (Maybe Word) (Maybe Word)        -- [a:b], a is the first, b is the maximum, not upper bound
    | WidthStart (Maybe Word) StartParsed
    deriving Show

data LocationW
    = LocationParsedWord WordWidth StartParsed
    deriving Show

data LocationResolved
    = ResolvedWidthStart Word Word
    | ResolvedFields     Word (NonEmpty (Word, Text))
    | ResolvedPeriodic   Word Word Word Word

-- type family Location l t = r | r -> l where
--     Location Parsed MLayoutMemory = LocationMB
--     Location Parsed MLayoutWord   = LocationW
--     Location Parsed MLayoutBits   = LocationMB
--     Location Resolved a = (Location Parsed a, LocationResolved)

data ValueItem = ValueItem Integer Text Text -- value, name, doc

data MemoryItemParsed
    = MemoryItemMemory MLayoutMemory
    | MemoryItemWord MLayoutWord

data WordItemParsed
    = WordItemWord MLayoutWord
    | WordItemBits MLayoutBits
    | WordItemValue ValueItem

data BitsItemParsed
    = BitsItemBits MLayoutBits
    | BitsItemValue ValueItem

-- type family ItemType x where
--     ItemType MLayoutMemory = MemoryItem
--     ItemType MLayoutWord   = WordItem
--     ItemType MLayoutBits   = BitsItem

-- class HFunctor (h :: (* -> *) -> * -> *) where
--     hfmap :: (f :~> g) -> h f :~> h g

-- class HFunctor' (h :: (* -> *) -> *) where
--     hfmap' :: (f :~> g) -> h f -> h g
--
-- instance HFunctor' MemoryItem where
--     hfmap' f (MemoryItemMemory x) = MemoryItemMemory $ f x
--     hfmap' f (MemoryItemWord x) = MemoryItemWord $ f x
--
-- instance HFunctor' WordItem where
--     hfmap' f (WordItemWord x) = WordItemWord $ f x
--     hfmap' f (WordItemBits x) = WordItemBits $ f x
--     hfmap' _ (WordItemValue x) = WordItemValue x
--
-- instance HFunctor' BitsItem where
--     hfmap' f (BitsItemBits x) = BitsItemBits $ f x
--     hfmap' _ (BitsItemValue x) = BitsItemValue x
--
-- class HTraversable' (h :: (* -> *) -> *) where
--     htraverse' :: (Applicative f) => NatM f a b -> h a -> f (h b)
--
-- instance HTraversable' MemoryItem where
--     htraverse' f (MemoryItemMemory x) = MemoryItemMemory <$> f x
--     htraverse' f (MemoryItemWord x) = MemoryItemWord <$> f x
--
-- instance HTraversable' WordItem where
--     htraverse' f (WordItemWord x) = WordItemWord <$> f x
--     htraverse' f (WordItemBits x) = WordItemBits <$> f x
--     htraverse' _ (WordItemValue x) = pure $ WordItemValue x
--
-- instance HTraversable' BitsItem where
--     htraverse' f (BitsItemBits x) = BitsItemBits <$> f x
--     htraverse' _ (BitsItemValue x) = pure $ BitsItemValue x

data MLayoutMemory = MLayoutMemory LocationMB Text Text [MemoryItemParsed]
data MLayoutWord   = MLayoutWord   LocationW  Text Text [WordItemParsed]
data MLayoutBits   = MLayoutBits   LocationMB Text Text [BitsItemParsed]

-- type MLayout = HFix (MLayoutF Parsed) MLayoutMemory
--
-- type MLayoutParsed = HFix (MLayoutF Parsed) MLayoutMemory
-- type WLayoutParsed = HFix (MLayoutF Parsed)  MLayoutWord
-- type BLayoutParsed = HFix (MLayoutF Parsed)  MLayoutBits
--
-- type MemoryItemParsed = MemoryItem (HFix (MLayoutF Parsed))
-- -- type MemoryItemResolved = MemoryItem (HFix (MLayoutF Resolved))

data MemoryItemResolved;

-- type WordItemParsed = WordItem (HFix (MLayoutF Parsed))
-- type BitsItemParsed = BitsItem (HFix (MLayoutF Parsed))
--
-- instance HFunctor (MLayoutF l) where
--     hfmap f (MLayoutMemoryF l n d mis) = MLayoutMemoryF l n d $ fmap (hfmap' f) mis
--     hfmap f (MLayoutWordF   l n d wis) = MLayoutWordF   l n d $ fmap (hfmap' f) wis
--     hfmap f (MLayoutBitsF   l n d bis) = MLayoutBitsF   l n d $ fmap (hfmap' f) bis
--
-- instance HFoldable (MLayoutF l) where
--     hfoldMap _ = undefined -- isn't used; we need HFoldable for HTraversable; it can be deduced from HTraversable
--
-- instance HTraversable (MLayoutF l) where
--     htraverse f (MLayoutMemoryF l n d mis) = MLayoutMemoryF l n d <$> (sequenceA $ fmap (htraverse' f) mis)
--     htraverse f (MLayoutWordF   l n d wis) = MLayoutWordF   l n d <$> (sequenceA $ fmap (htraverse' f) wis)
--     htraverse f (MLayoutBitsF   l n d bis) = MLayoutBitsF   l n d <$> (sequenceA $ fmap (htraverse' f) bis)
--
-- class BuildableLayout x where
--     mk :: Location l x -> Text -> Text -> [(ItemType x) (HFix (MLayoutF l))] -> HFix (MLayoutF l) x
--
-- instance BuildableLayout MLayoutMemory where
--     mk l n d is = HFix (MLayoutMemoryF l n d is)
--
-- instance BuildableLayout MLayoutWord where
--     mk l n d is = HFix (MLayoutWordF l n d is)
--
-- instance BuildableLayout MLayoutBits where
--     mk l n d is = HFix (MLayoutBitsF l n d is)

--------------------------------------------------------------------

prettyMaybe :: Pretty w => Maybe w -> Doc ann
prettyMaybe (Just x) = pretty x
prettyMaybe Nothing = mempty

instance Pretty ValueItem where
    pretty (ValueItem v n d) = "=" <> pretty v <+> pretty n <+> dquotes (pretty d)

instance Pretty WordWidth where
    pretty W8   = "%8"
    pretty W16  = "%16"
    pretty W32  = "%32"
    pretty W64  = "%64"
    pretty W128 = "%128"

instance Pretty StartParsed where
    pretty  Next = mempty
    pretty (Simple w) = pretty w
    pretty (Fields pairs) = braces $ cat $ punctuate ", " $ toList $ fmap posPretty pairs
        where
            posPretty (mat, name) = maybe mempty ((<> space) . pretty) mat <> pretty name
    pretty (Periodic mat n ms) = prettyMaybe mat <> brackets (pretty n <> maybe mempty appendStep ms)
        where
            appendStep w = space <> "+" <> pretty w

instance Pretty MemoryItemParsed where
    pretty (MemoryItemMemory i) = pretty i
    pretty (MemoryItemWord i)   = pretty i

instance Pretty WordItemParsed where
    pretty (WordItemWord i)  = pretty i
    pretty (WordItemBits i)  = pretty i
    pretty (WordItemValue i) = pretty i

instance Pretty BitsItemParsed where
    pretty (BitsItemBits i)  = pretty i
    pretty (BitsItemValue i) = pretty i

prettyDoc :: Text -> Doc ann
prettyDoc t = if Data.Text.null t
                  then mempty
                  else space <> dquotes (pretty t)

{-
instance Pretty LocationResolved where
    pretty (ResolvedWidthStart w at)      = pretty w <> "@" <> pretty at
    pretty (ResolvedFields w pairs)       = pretty w <> "@" <> braces (cat $ punctuate ", " $ toList $ fmap posPretty pairs)
        where
            posPretty (at, name) = pretty at <+> pretty name
    pretty (ResolvedPeriodic w at n step) = pretty w <> "@" <> pretty at <> brackets (pretty n <+> "+" <> pretty step)

class PrettyLocation l where
    prettyLocationM :: Location l MLayoutMemory -> Doc ann
    prettyLocationW :: Location l MLayoutWord -> Doc ann
    prettyLocationB :: Location l MLayoutBits -> Doc ann

instance PrettyLocation Parsed where
-}

prettyLocationM :: LocationMB -> Doc ann
prettyLocationM (FromTo mwFrom mwTo) = prettyMaybe mwFrom <> ":" <> prettyMaybe mwTo
prettyLocationM (WidthStart Nothing Next) = mempty
prettyLocationM (WidthStart mw Next) = prettyMaybe mw
prettyLocationM (WidthStart mw sp) = prettyMaybe mw <> "@" <> pretty sp

prettyLocationW :: LocationW -> Doc ann
prettyLocationW (LocationParsedWord w Next) = pretty w
prettyLocationW (LocationParsedWord w sp) = pretty w <> "@" <> pretty sp

prettyLocationB :: LocationMB -> Doc ann
prettyLocationB (WidthStart Nothing Next) = mempty
prettyLocationB (WidthStart Nothing sp) = pretty sp
prettyLocationB (WidthStart (Just w) Next) = pretty w <> "@"
prettyLocationB l = prettyLocationM l

{-
instance PrettyLocation Resolved where
    prettyLocationM (_, l) = pretty l
    prettyLocationW (_, l) = pretty l
    prettyLocationB (_, l) = pretty l

prettyAlg :: PrettyLocation l => MLayoutF l (Const (Doc ann)) :~> (Const (Doc ann))

-}

prettySubitems :: Pretty c => [c] -> Doc ann
prettySubitems [] = mempty
prettySubitems is = space <> braces (line <> indent 4 (vsep $ fmap pretty is) <> line)

instance Pretty MLayoutMemory where
    pretty (MLayoutMemory l n d mis) = brackets (prettyLocationM l) <+> pretty n <> prettyDoc d <> prettySubitems mis
instance Pretty MLayoutWord where
    pretty (MLayoutWord   l n d wis) = brackets (prettyLocationW l) <+> pretty n <> prettyDoc d <> prettySubitems wis
instance Pretty MLayoutBits where
    pretty (MLayoutBits   l n d bis) = angles   (prettyLocationB l) <+> pretty n <> prettyDoc d <> prettySubitems bis

{-
instance PrettyLocation l => Pretty (MemoryItem (HFix (MLayoutF l))) where
    pretty (MemoryItemMemory x) = getConst $ hcata prettyAlg x
    pretty (MemoryItemWord x)   = getConst $ hcata prettyAlg x

-}
instance Pretty MemoryItemResolved where
    pretty _ = mempty
