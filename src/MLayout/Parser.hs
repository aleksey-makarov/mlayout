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

import           MLayout.Data

------------------------------------------------------------------------

data StartP
    = NextP                                    -- [%32@], [%32], [5@], [5] NB: [2] means [2@], BUT <2> means <@2>
    | SimpleP Word                             -- [@0x11], [7@0x22], [%16@3]
    | FieldsP (NonEmpty (Maybe Word, Text))    -- [@{A, B, C}], [%12@{12 A, 34 B}]
    | PeriodicP (Maybe Word) Word (Maybe Word) -- [1@2[3 +4]] means optional start, mandatory number of items (>= 2), optional step
    deriving Show

instance Pretty StartP where
    pretty  NextP = mempty
    pretty (SimpleP w) = pretty w
    pretty (FieldsP pairs) = PP.braces $ cat $ punctuate ", " $ NEL.toList $ fmap posPretty pairs
        where
            posPretty (mat, name) = maybe mempty ((<> PP.space) . pretty) mat <> pretty name
    pretty (PeriodicP mat n ms) = prettyMaybe mat <> PP.brackets (pretty n <> maybe mempty appendStep ms)
        where
            appendStep w = PP.space <> "+" <> pretty w

data LocationMB
    = FromToP (Maybe Word) (Maybe Word)        -- [a:b], a is the first, b is the maximum, not upper bound
    | WidthStartP (Maybe Word) StartP
    deriving Show

data LocationW
    = LocationWordP WordWidth StartP
    deriving Show

data MemoryItemP
    = MemoryItemMemoryP MLayoutMemoryP
    | MemoryItemWordP MLayoutWordP

instance Pretty MemoryItemP where
    pretty (MemoryItemMemoryP i) = pretty i
    pretty (MemoryItemWordP i)   = pretty i

data WordItemP
    = WordItemWordP MLayoutWordP
    | WordItemBitsP MLayoutBitsP
    | WordItemValueP ValueItem

instance Pretty WordItemP where
    pretty (WordItemWordP i)  = pretty i
    pretty (WordItemBitsP i)  = pretty i
    pretty (WordItemValueP i) = pretty i

data BitsItemP
    = BitsItemBitsP MLayoutBitsP
    | BitsItemValueP ValueItem

instance Pretty BitsItemP where
    pretty (BitsItemBitsP i)  = pretty i
    pretty (BitsItemValueP i) = pretty i

data MLayoutMemoryP = MLayoutMemoryP LocationMB Text Text [MemoryItemP]

instance Pretty MLayoutMemoryP where
    pretty (MLayoutMemoryP l n d mis) = PP.brackets (prettyLocationM l) <+> pretty n <> prettyDoc d <> prettySubitems mis

data MLayoutWordP = MLayoutWordP LocationW Text Text [WordItemP]

instance Pretty MLayoutWordP where
    pretty (MLayoutWordP l n d wis) = PP.brackets (prettyLocationW l) <+> pretty n <> prettyDoc d <> prettySubitems wis

data MLayoutBitsP = MLayoutBitsP LocationMB Text Text [BitsItemP]

instance Pretty MLayoutBitsP where
    pretty (MLayoutBitsP l n d bis) = PP.angles (prettyLocationB l) <+> pretty n <> prettyDoc d <> prettySubitems bis

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
prettyLocationM (FromToP mwFrom mwTo) = prettyMaybe mwFrom <> ":" <> prettyMaybe mwTo
prettyLocationM (WidthStartP Nothing NextP) = mempty
prettyLocationM (WidthStartP mw NextP) = prettyMaybe mw
prettyLocationM (WidthStartP mw sp) = prettyMaybe mw <> "@" <> pretty sp

prettyLocationW :: LocationW -> Doc ann
prettyLocationW (LocationWordP w NextP) = pretty w
prettyLocationW (LocationWordP w sp) = pretty w <> "@" <> pretty sp

prettyLocationB :: LocationMB -> Doc ann
prettyLocationB (WidthStartP Nothing NextP) = mempty
prettyLocationB (WidthStartP Nothing sp) = pretty sp
prettyLocationB (WidthStartP (Just w) NextP) = pretty w <> "@"
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

startSimpleP :: Maybe Word -> Prsr StartP
startSimpleP maybeStart = return $ maybe NextP SimpleP maybeStart

startArrayP :: Maybe Word -> Prsr StartP
startArrayP maybeStart = Tok.brackets (PeriodicP maybeStart <$> wordP <*> optional ((symbolic '+') *> wordP))

startArrayOrSimpleP :: Prsr StartP
startArrayOrSimpleP = do
    x <- optional wordP
    startArrayP x <|> startSimpleP x

startSetP :: Prsr StartP
startSetP = FieldsP <$> (Tok.braces $ sepByNonEmpty ((,) <$> optional wordP <*> nameP) (symbolic ','))

atP :: Prsr StartP
atP = symbolic '@' *> (startSetP <|> startArrayOrSimpleP)

fromToP :: Maybe Word -> Prsr LocationMB
fromToP maybeFrom = FromToP maybeFrom <$> (symbolic ':' *> optional wordP)

fromToOrAtP :: Maybe Word -> Prsr LocationMB
fromToOrAtP x = fromToP x <|> (WidthStartP x <$> atP)

justOneWordLocationBits :: Word -> LocationMB
justOneWordLocationBits w = WidthStartP Nothing (SimpleP w)

justOneWordLocationMemory :: Word -> LocationMB
justOneWordLocationMemory w = WidthStartP (Just w) NextP

locationInternalsP :: (Word -> LocationMB) -> Prsr LocationMB
locationInternalsP justOneWord = do
    x <- optional wordP
    fromToOrAtP x <|> (return $ maybe (WidthStartP Nothing NextP) justOneWord x)

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
wLocationInternalsP = LocationWordP <$> wWidthP <*> (maybe NextP id <$> optional atP)

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

bLayoutP :: Prsr BitsItemP
bLayoutP = (BitsItemBitsP <$> (MLayoutBitsP <$> bLocationP <*> nameP <*> docP <*> maybeSubitems bLayoutsP)) <|> (BitsItemValueP <$> vItemP)

bLayoutsP :: Prsr [BitsItemP]
bLayoutsP = some bLayoutP

wLayoutP :: Prsr WordItemP
wLayoutP =  (WordItemWordP <$> (MLayoutWordP <$> wLocationP <*> nameP <*> docP <*> maybeSubitems wLayoutsP))
        <|> (WordItemBitsP <$> (MLayoutBitsP <$> bLocationP <*> nameP <*> docP <*> maybeSubitems bLayoutsP))
        <|> (WordItemValueP <$> vItemP)

wLayoutsP :: Prsr [WordItemP]
wLayoutsP = some wLayoutP

mLayoutP :: Prsr MemoryItemP
mLayoutP = mwLocationP >>= either mmLayoutP mwLayoutP
    where
        mmLayoutP l = MemoryItemMemoryP <$> (MLayoutMemoryP l <$> nameP <*> docP <*> maybeSubitems mLayoutsP)
        mwLayoutP l = MemoryItemWordP   <$> (MLayoutWordP   l <$> nameP <*> docP <*> maybeSubitems wLayoutsP)

mLayoutsP :: Prsr [MemoryItemP]
mLayoutsP = some mLayoutP

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype Prsr a = Prsr { runPrsr :: Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, Errable)

instance TokenParsing Prsr where
    someSpace = buildSomeSpaceParser (Prsr someSpace) $ CommentStyle "" "" "#" True
-- use the default implementation for other methods:
-- nesting, semi, highlight, token

parser :: Parser [MemoryItemP]
parser = runPrsr $ whiteSpace *> mLayoutsP <* eof
