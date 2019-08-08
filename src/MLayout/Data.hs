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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wall #-}

module MLayout.Data where

import Data.Text
import Data.List.NonEmpty
import Data.Text.Prettyprint.Doc

import MLayout.HFunctor

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

-- (* -> *) -> *
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

type MLayout = HFix (MLayoutF Location) MLayoutMemory

type MLayoutParsed = HFix (MLayoutF LocationParsed) MLayoutMemory
type WLayoutParsed = HFix (MLayoutF LocationParsed) MLayoutWord
type BLayoutParsed = HFix (MLayoutF LocationParsed) MLayoutBits

type MemoryItemParsed = MemoryItem (HFix (MLayoutF LocationParsed))
type MemoryItemResolved = MemoryItem (HFix (MLayoutF Location)) -- FIXME

type WordItemParsed = WordItem (HFix (MLayoutF LocationParsed))
type BitsItemParsed = BitsItem (HFix (MLayoutF LocationParsed))

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

mkM :: ItemDescription l -> [MemoryItem (HFix (MLayoutF l))] -> HFix (MLayoutF l) MLayoutMemory
mkM i l = HFix (MLayoutMemoryF i l)

mkW :: ItemDescription l -> [WordItem (HFix (MLayoutF l))] -> HFix (MLayoutF l) MLayoutWord
mkW i l = HFix (MLayoutWordF i l)

mkB :: ItemDescription l -> [BitsItem (HFix (MLayoutF l))] -> HFix (MLayoutF l) MLayoutBits
mkB i l = HFix (MLayoutBitsF i l)

instance Pretty (HFix (MLayoutF LocationParsed) d) where
    pretty = undefined

instance Pretty MemoryItemParsed where
    pretty (MemoryItemMemory x) = pretty x
    pretty (MemoryItemWord x)   = pretty x

instance Pretty MemoryItemResolved where
    pretty = undefined

{-
--------------------------------------------------------------------------------

prettyMaybe :: Pretty w => Maybe w -> Doc ann
prettyMaybe (Just x) = pretty x
prettyMaybe Nothing = mempty

instance Pretty w => Pretty (Location w) where
    -- FromTo (Maybe Word) (Maybe Word)
    pretty (FromTo mf mt) = prettyMaybe mf <> ":" <> prettyMaybe mt
    -- WordNext w
    pretty (WordNext w) = pretty w <> "@"
    -- Word (Maybe w) Word
    pretty (Word mw at) = prettyMaybe mw <> "@" <> pretty at
    -- Fields (Maybe w) (NonEmpty (Maybe Word, Text))
    pretty (Fields mw pairs) = prettyMaybe mw <> "@" <> (PPD.braces $ PPD.cat $ punctuate ", " $ LNE.toList $ fmap posPretty pairs)
        where
            posPretty (mat, name) = prettyMaybe mat <+> pretty name
    -- Periodic (Maybe w) (Maybe Word) Word (Maybe Word)
    pretty (Periodic mw mat n ms) = prettyMaybe mw <> "@" <> prettyMaybe mat <> PPD.brackets (pretty n <> maybe mempty appendStep ms)
        where
            appendStep w = PPD.space <> "+" <> pretty w

prettyDoc :: Text -> Doc ann
prettyDoc t = if Data.Text.null t
                  then mempty
                  else PPD.space <> dquotes (pretty t)

instance Pretty ValueItem where
    pretty (ValueItem v n d) = "=" <> pretty v <+> pretty n <+> dquotes (pretty d)

instance Pretty (Item BLocation ()) where
    pretty (Item l n d ()) = PPD.angles (pretty l) <+> pretty n <> prettyDoc d

instance Pretty (Item MLocation BLayout) where
    pretty (Item l n d _) = PPD.brackets (pretty l) <+> pretty n <> prettyDoc d

class PrettyInternals a where
    prettyInternals :: a -> Doc ann
    prettyInternalsIsNull :: a -> Bool

instance PrettyInternals (Item MLocation BLayout) where
    prettyInternals (Item _ _ _ b) = pretty b
    prettyInternalsIsNull (Item _ _ _ b) = XTree.null b

prettyBLayoutAlg :: (Pretty l, Pretty n) => XTreeF l n (XTree l n, Doc ann) -> Doc ann
prettyBLayoutAlg (XTreeF ls) = PPD.vsep $ fmap (either pretty prettyr) ls
    where
        prettyr (item, (subtree, subtreeFormatted)) =
            pretty item <> if XTree.null subtree
                               then mempty
                               else PPD.space <> PPD.braces (line <> indent 4 subtreeFormatted <> line)

instance (Pretty l, Pretty n) => Pretty (XTree l n) where
    pretty = para prettyBLayoutAlg

prettyMLayoutAlg :: (Pretty n, PrettyInternals n) => TreeF n (Doc ann) -> Doc ann
prettyMLayoutAlg (TreeF item s) = pretty item <> prettyBody
    where
        prettyBody = if prettyInternalsIsNull item && P.null s
                         then mempty
                         else PPD.space <> PPD.braces (line <> indent 4 (prettyInternals item <> sepbm <> PPD.vsep s) <> line)
        sepbm = if prettyInternalsIsNull item || P.null s
                    then mempty
                    else line

instance (Pretty n, PrettyInternals n) => Pretty (Tree n) where
    pretty = cata prettyMLayoutAlg

--------------------------------------------------------------------------------

-}
