{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

module MLayout.Data
    ( WordWidth (..)
    , ValueItem (..)
    ) where

-- import Control.Applicative
import Data.List.NonEmpty
import Data.Text
import Prettyprinter

data WordWidth = W8 | W16 | W32 | W64 | W128 deriving Show

instance Pretty WordWidth where
    pretty W8   = "%8"
    pretty W16  = "%16"
    pretty W32  = "%32"
    pretty W64  = "%64"
    pretty W128 = "%128"

data Start
    = Start Word                     -- [@0x11], [7@0x22], [%16@3]
    | Fields (NonEmpty (Word, Text)) -- [@{A, B, C}], [%12@{12 A, 34 B}]
    | Periodic Word Word Word        -- [1@2[3 +4]] means optional start, mandatory number of items (>= 2), optional step
    deriving Show

instance Pretty Start where
    pretty (Start w) = pretty w
    pretty (Fields pairs) = braces $ cat $ punctuate ", " $ toList $ fmap posPretty pairs
        where
            posPretty (at, name) = pretty at <+> pretty name
    pretty (Periodic at n s) = pretty at <> brackets (pretty n <+> "+" <> pretty s)

data ValueItem = ValueItem Integer Text Text -- =value name doc

instance Pretty ValueItem where
    pretty (ValueItem v n d) = "=" <> pretty v <+> pretty n <+> dquotes (pretty d)

{-

data MemoryItemP
    = MemoryItemMemory MLayoutMemory
    | MemoryItemWord MLayoutWord

instance Pretty MemoryItemP where
    pretty (MemoryItemMemory i) = pretty i
    pretty (MemoryItemWord i)   = pretty i

data WordItemParsed
    = WordItemWord MLayoutWord
    | WordItemBits MLayoutBits
    | WordItemValue ValueItem

instance Pretty WordItemParsed where
    pretty (WordItemWord i)  = pretty i
    pretty (WordItemBits i)  = pretty i
    pretty (WordItemValue i) = pretty i

data BitsItemParsed
    = BitsItemBits MLayoutBits
    | BitsItemValue ValueItem

instance Pretty BitsItemParsed where
    pretty (BitsItemBits i)  = pretty i
    pretty (BitsItemValue i) = pretty i

data MLayoutMemory = MLayoutMemory LocationMB Text Text [MemoryItemP]

instance Pretty MLayoutMemory where
    pretty (MLayoutMemory l n d mis) = PP.brackets (prettyLocationM l) <+> pretty n <> prettyDoc d <> prettySubitems mis

data MLayoutWord = MLayoutWord LocationW Text Text [WordItemParsed]

instance Pretty MLayoutWord where
    pretty (MLayoutWord l n d wis) = PP.brackets (prettyLocationW l) <+> pretty n <> prettyDoc d <> prettySubitems wis

data MLayoutBits = MLayoutBits LocationMB Text Text [BitsItemParsed]

instance Pretty MLayoutBits where
    pretty (MLayoutBits l n d bis) = PP.angles (prettyLocationB l) <+> pretty n <> prettyDoc d <> prettySubitems bis

-}
