{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wall #-}

module MLayout.Data
    ( WordWidth (..)
    , ValueItem (..)
    ) where

-- import Control.Applicative
import Data.List.NonEmpty as NEL
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

data MemoryItem
    = MemoryItemMemory MLayoutMemory
    | MemoryItemWord MLayoutWord

instance Pretty MemoryItem where
    pretty (MemoryItemMemory i) = pretty i
    pretty (MemoryItemWord i)   = pretty i

data WordItem
    = WordItemWord MLayoutWord
    | WordItemBits MLayoutBits
    | WordItemValue ValueItem

instance Pretty WordItem where
    pretty (WordItemWord i)  = pretty i
    pretty (WordItemBits i)  = pretty i
    pretty (WordItemValue i) = pretty i

data BitsItem
    = BitsItemBits MLayoutBits
    | BitsItemValue ValueItem

instance Pretty BitsItem where
    pretty (BitsItemBits i)  = pretty i
    pretty (BitsItemValue i) = pretty i

data MLayoutMemory = MLayoutMemory
    { mStart :: Start
    , mWidth :: Word
    , mName  :: Text
    , mDoc   :: Text
    , mItems :: [MemoryItem]
    }

instance Pretty MLayoutMemory where
    pretty MLayoutMemory { .. } = brackets (pretty mWidth <> "@" <> prettyStart mStart) <+> pretty mName <> prettyDoc mDoc <> prettySubitems mItems

prettyStart :: Start -> Doc ann
prettyStart (Start start) = pretty start
prettyStart (Fields pairs) = braces $ cat $ punctuate ", " $ NEL.toList $ fmap posPretty pairs
    where
            posPretty (at, name) = pretty at <+> pretty name
prettyStart (Periodic start n step) = pretty start <> brackets (pretty n <+> "+" <> pretty step)

data MLayoutWord = MLayoutWord
    { wStart :: Start
    , wWidth :: WordWidth
    , wName  :: Text
    , wDoc   :: Text
    , wItems :: [MemoryItem]
    }

instance Pretty MLayoutWord where
    pretty MLayoutWord { .. } = brackets (pretty wWidth <> "@" <> prettyStart wStart) <+> pretty wName <> prettyDoc wDoc <> prettySubitems wItems

data MLayoutBits = MLayoutBits
    { bStart :: Start
    , bWidth :: Word
    , bName  :: Text
    , bDoc   :: Text
    , bItems :: [MemoryItem]
    }

instance Pretty MLayoutBits where
    pretty MLayoutBits { .. } = angles (prettyLocationB bStart) <+> pretty bName <> prettyDoc bDoc <> prettySubitems bItems
        where
            prettyLocationB (Start start) = pretty start <> if bWidth == 1 then mempty else ":" <> pretty (start + bWidth - 1)
            prettyLocationB _ = pretty bWidth <> "@" <> prettyStart bStart

prettyDoc :: Text -> Doc ann
prettyDoc t = if Data.Text.null t
                  then mempty
                  else space <> dquotes (pretty t)

prettySubitems :: Pretty c => [c] -> Doc ann
prettySubitems [] = mempty
prettySubitems is = space <> braces (line <> indent 4 (vsep $ fmap pretty is) <> line)
