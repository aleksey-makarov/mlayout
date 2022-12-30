{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    pretty MLayoutMemory { .. } = brackets (prettyLocation mStart mWidth) <+> pretty mName <> prettyDoc mDoc <> prettySubitems mItems
        where
            prettyLocation (Start start)           width = pretty width <> "@" <> pretty start
            prettyLocation (Fields nel)            width = undefined
            prettyLocation (Periodic start n step) width = undefined

--    pretty (FieldsP pairs) = PP.braces $ cat $ punctuate ", " $ NEL.toList $ fmap posPretty pairs
--        where
--            posPretty (mat, name) = maybe mempty ((<> PP.space) . pretty) mat <> pretty name
--    pretty (PeriodicP mat n ms) = prettyMaybe mat <> PP.brackets (pretty n <> maybe mempty appendStep ms)
--        where
--            appendStep w = PP.space <> "+" <> pretty w


-- prettyLocationM :: LocationMB -> Doc ann
-- prettyLocationM (FromToP mwFrom mwTo) = prettyMaybe mwFrom <> ":" <> prettyMaybe mwTo
-- prettyLocationM (WidthStartP Nothing NextP) = mempty
-- prettyLocationM (WidthStartP mw NextP) = prettyMaybe mw
-- prettyLocationM (WidthStartP mw sp) = prettyMaybe mw <> "@" <> pretty sp

data MLayoutWord = MLayoutWord
    { wStart :: Start
    , wWidth :: Word
    , wName  :: Text
    , wDoc   :: Text
    , wItems :: [MemoryItem]
    }

instance Pretty MLayoutWord where
    pretty MLayoutWord { .. } = undefined

data MLayoutBits = MLayoutBits
    { bStart :: Start
    , bWidth :: Word
    , bName  :: Text
    , bDoc   :: Text
    , bItems :: [MemoryItem]
    }

instance Pretty MLayoutBits where
    pretty MLayoutBits { .. } = undefined

prettyDoc :: Text -> Doc ann
prettyDoc t = if Data.Text.null t
                  then mempty
                  else space <> dquotes (pretty t)

prettySubitems :: Pretty c => [c] -> Doc ann
prettySubitems [] = mempty
prettySubitems is = space <> braces (line <> indent 4 (vsep $ fmap pretty is) <> line)

--
-- data MLayoutWord = MLayoutWord LocationW Text Text [WordItemParsed]
--
-- instance Pretty MLayoutWord where
--     pretty (MLayoutWord l n d wis) = PP.brackets (prettyLocationW l) <+> pretty n <> prettyDoc d <> prettySubitems wis
--
-- data MLayoutBits = MLayoutBits LocationMB Text Text [BitsItemParsed]
--
-- instance Pretty MLayoutBits where
--     pretty (MLayoutBits l n d bis) = PP.angles (prettyLocationB l) <+> pretty n <> prettyDoc d <> prettySubitems bis
