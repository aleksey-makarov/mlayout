{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MLayout.Parser
    ( parser
    ) where

import           Prelude as P
import           Control.Applicative
import           Control.Monad
import           Data.HashSet as HS
import           Data.Text hiding (maximum)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import           Data.List.NonEmpty as NEL
import           Formatting (Format, runFormat, int, (%))
import           Prettyprinter as PP
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token as Tok
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

------------------------------------------------------------------------

data WordWidth = W8 | W16 | W32 | W64 | W128 deriving Show

instance Pretty WordWidth where
    pretty W8   = "%8"
    pretty W16  = "%16"
    pretty W32  = "%32"
    pretty W64  = "%64"
    pretty W128 = "%128"

data StartParsed
    = Next                                    -- [%32@], [%32], [5@], [5] NB: [2] means [2@], BUT <2> means <@2>
    | Simple Word                             -- [@0x11], [7@0x22], [%16@3]
    | Fields (NonEmpty (Maybe Word, Text))    -- [@{A, B, C}], [%12@{12 A, 34 B}]
    | Periodic (Maybe Word) Word (Maybe Word) -- [1@2[3 +4]] means optional start, mandatory number of items (>= 2), optional step
    deriving Show

instance Pretty StartParsed where
    pretty  Next = mempty
    pretty (Simple w) = pretty w
    pretty (Fields pairs) = PP.braces $ cat $ punctuate ", " $ NEL.toList $ fmap posPretty pairs
        where
            posPretty (mat, name) = maybe mempty ((<> PP.space) . pretty) mat <> pretty name
    pretty (Periodic mat n ms) = prettyMaybe mat <> PP.brackets (pretty n <> maybe mempty appendStep ms)
        where
            appendStep w = PP.space <> "+" <> pretty w

data LocationMB
    = FromTo (Maybe Word) (Maybe Word)        -- [a:b], a is the first, b is the maximum, not upper bound
    | WidthStart (Maybe Word) StartParsed
    deriving Show

data LocationW
    = LocationParsedWord WordWidth StartParsed
    deriving Show

data ValueItem = ValueItem Integer Text Text -- value, name, doc

instance Pretty ValueItem where
    pretty (ValueItem v n d) = "=" <> pretty v <+> pretty n <+> dquotes (pretty d)

data MemoryItemParsed
    = MemoryItemMemory MLayoutMemory
    | MemoryItemWord MLayoutWord

instance Pretty MemoryItemParsed where
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

data MLayoutMemory = MLayoutMemory LocationMB Text Text [MemoryItemParsed]

instance Pretty MLayoutMemory where
    pretty (MLayoutMemory l n d mis) = PP.brackets (prettyLocationM l) <+> pretty n <> prettyDoc d <> prettySubitems mis

data MLayoutWord = MLayoutWord LocationW Text Text [WordItemParsed]

instance Pretty MLayoutWord where
    pretty (MLayoutWord l n d wis) = PP.brackets (prettyLocationW l) <+> pretty n <> prettyDoc d <> prettySubitems wis

data MLayoutBits = MLayoutBits LocationMB Text Text [BitsItemParsed]

instance Pretty MLayoutBits where
    pretty (MLayoutBits l n d bis) = PP.angles (prettyLocationB l) <+> pretty n <> prettyDoc d <> prettySubitems bis

prettyDoc :: Text -> Doc ann
prettyDoc t = if Data.Text.null t
                  then mempty
                  else PP.space <> dquotes (pretty t)

prettyMaybe :: Pretty w => Maybe w -> Doc ann
prettyMaybe (Just x) = pretty x
prettyMaybe Nothing = mempty

prettySubitems :: Pretty c => [c] -> Doc ann
prettySubitems [] = mempty
prettySubitems is = PP.space <> PP.braces (line <> indent 4 (vsep $ fmap pretty is) <> line)

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

------------------------------------------------------------------------

throw :: (Applicative m, Errable m) => Format (m b) a -> a
throw f = runFormat f $ raiseErr . failed . TL.unpack . TLB.toLazyText

wordP :: forall a m . (TokenParsing m, Errable m, Monad m, Num a, Integral a, Bounded a) => m a
wordP = do
    v <- natural
    if v < toInteger (minBound :: a) || toInteger (maxBound :: a) < v
        -- FIXME: in the error message use source number representation
        then throw ("should be " % int % " .. " % int) (minBound :: a) (maxBound :: a)
        else return $ fromInteger v

startSimpleP :: Maybe Word -> Prsr StartParsed
startSimpleP maybeStart = return $ maybe Next Simple maybeStart

startArrayP :: Maybe Word -> Prsr StartParsed
startArrayP maybeStart = Tok.brackets (Periodic maybeStart <$> wordP <*> optional ((symbolic '+') *> wordP))

startArrayOrSimpleP :: Prsr StartParsed
startArrayOrSimpleP = do
    x <- optional wordP
    startArrayP x <|> startSimpleP x

startSetP :: Prsr StartParsed
startSetP = Fields <$> (Tok.braces $ sepByNonEmpty ((,) <$> optional wordP <*> nameP) (symbolic ','))

atP :: Prsr StartParsed
atP = symbolic '@' *> (startSetP <|> startArrayOrSimpleP)

fromToP :: Maybe Word -> Prsr LocationMB
fromToP maybeFrom = FromTo maybeFrom <$> (symbolic ':' *> optional wordP)

fromToOrAtP :: Maybe Word -> Prsr LocationMB
fromToOrAtP x = fromToP x <|> (WidthStart x <$> atP)

justOneWordLocationBits :: Word -> LocationMB
justOneWordLocationBits w = WidthStart Nothing (Simple w)

justOneWordLocationMemory :: Word -> LocationMB
justOneWordLocationMemory w = WidthStart (Just w) Next

locationInternalsP :: (Word -> LocationMB) -> Prsr LocationMB
locationInternalsP justOneWord = do
    x <- optional wordP
    fromToOrAtP x <|> (return $ maybe (WidthStart Nothing Next) justOneWord x)

wWidthP :: Prsr WordWidth
wWidthP = token (char '%' *> (  W8   <$ string "8"
                            <|> W16  <$ string "16"
                            <|> W32  <$ string "32"
                            <|> W64  <$ string "64"
                            <|> W128 <$ string "128" ))

mLocationInternalsP :: Prsr LocationMB
mLocationInternalsP = locationInternalsP justOneWordLocationMemory

-- [%12@..], [%12]
wLocationInternalsP :: Prsr LocationW
wLocationInternalsP = LocationParsedWord <$> wWidthP <*> (maybe Next id <$> optional atP)

bLocationInternalsP :: Prsr LocationMB
bLocationInternalsP = locationInternalsP justOneWordLocationBits

-- <12:12> <|> <12@..>
bLocationP :: Prsr LocationMB
bLocationP = Tok.angles bLocationInternalsP

-- [mWordP] <|> [:12] <|> [@12] <|> [12:12] <|> [12@..] <|> [12]
mwLocationP :: Prsr (Either LocationMB LocationW) -- left for memory, right for word
mwLocationP = Tok.brackets (Right <$> wLocationInternalsP <|> Left <$> mLocationInternalsP)

wLocationP :: Prsr LocationW
wLocationP = Tok.brackets wLocationInternalsP

nameP :: Prsr Text
nameP = ident (IdentifierStyle "Name Style" upper (alphaNum <|> oneOf "_'") HS.empty Identifier ReservedIdentifier) <?> "id"

docP :: Prsr Text
docP = (stringLiteral <|> return "") <?> "documentation string"

vItemP :: Prsr ValueItem
vItemP = (symbolic '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

maybeSubitems :: Prsr [a] -> Prsr [a]
maybeSubitems subitemParser = Tok.braces subitemParser <|> return []

bLayoutP :: Prsr BitsItemParsed
bLayoutP = (BitsItemBits <$> (MLayoutBits <$> bLocationP <*> nameP <*> docP <*> maybeSubitems bLayoutsP)) <|> (BitsItemValue <$> vItemP)

bLayoutsP :: Prsr [BitsItemParsed]
bLayoutsP = some bLayoutP

wLayoutP :: Prsr WordItemParsed
wLayoutP =  (WordItemWord <$> (MLayoutWord <$> wLocationP <*> nameP <*> docP <*> maybeSubitems wLayoutsP))
        <|> (WordItemBits <$> (MLayoutBits <$> bLocationP <*> nameP <*> docP <*> maybeSubitems bLayoutsP))
        <|> (WordItemValue <$> vItemP)

wLayoutsP :: Prsr [WordItemParsed]
wLayoutsP = some wLayoutP

mLayoutP :: Prsr MemoryItemParsed
mLayoutP = mwLocationP >>= either mmLayoutP mwLayoutP
    where
        mmLayoutP l = MemoryItemMemory <$> (MLayoutMemory l <$> nameP <*> docP <*> maybeSubitems mLayoutsP)
        mwLayoutP l = MemoryItemWord   <$> (MLayoutWord   l <$> nameP <*> docP <*> maybeSubitems wLayoutsP)

mLayoutsP :: Prsr [MemoryItemParsed]
mLayoutsP = some mLayoutP

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype Prsr a = Prsr { runPrsr :: Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, Errable)

instance TokenParsing Prsr where
    someSpace = buildSomeSpaceParser (Prsr someSpace) $ CommentStyle "" "" "#" True
-- use the default implementation for other methods:
-- nesting, semi, highlight, token

parser :: Parser [MemoryItemParsed]
parser = runPrsr $ whiteSpace *> mLayoutsP <* eof
