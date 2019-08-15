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
import           Formatting (Format, runFormat, int, (%))
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token as TPT
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           MLayout.Data

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
startArrayP maybeStart = TPT.brackets (Periodic maybeStart <$> wordP <*> optional ((symbolic '+') *> wordP))

startArrayOrSimpleP :: Prsr StartParsed
startArrayOrSimpleP = do
    x <- optional wordP
    startArrayP x <|> startSimpleP x

startSetP :: Prsr StartParsed
startSetP = Fields <$> (TPT.braces $ sepByNonEmpty ((,) <$> optional wordP <*> nameP) (symbolic ','))

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
bLocationP = TPT.angles bLocationInternalsP

-- [mWordP] <|> [:12] <|> [@12] <|> [12:12] <|> [12@..] <|> [12]
mwLocationP :: Prsr (Either LocationMB LocationW) -- left for memory, right for word
mwLocationP = TPT.brackets (Right <$> wLocationInternalsP <|> Left <$> mLocationInternalsP)

wLocationP :: Prsr LocationW
wLocationP = TPT.brackets wLocationInternalsP

nameP :: Prsr Text
nameP = ident (IdentifierStyle "Name Style" upper (alphaNum <|> oneOf "_'") HS.empty Identifier ReservedIdentifier) <?> "id"

docP :: Prsr Text
docP = (stringLiteral <|> return "") <?> "documentation string"

vItemP :: Prsr ValueItem
vItemP = (symbolic '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

maybeSubitems :: Prsr [a] -> Prsr [a]
maybeSubitems subitemParser = TPT.braces subitemParser <|> return []

bLayoutP :: Prsr BitsItemParsed
bLayoutP = (BitsItemBits <$> (mkB <$> bLocationP <*> nameP <*> docP <*> maybeSubitems bLayoutsP)) <|> (BitsItemValue <$> vItemP)

bLayoutsP :: Prsr [BitsItemParsed]
bLayoutsP = some bLayoutP

wLayoutP :: Prsr WordItemParsed
wLayoutP =  (WordItemWord <$> (mkW <$> wLocationP <*> nameP <*> docP <*> maybeSubitems wLayoutsP))
        <|> (WordItemBits <$> (mkB <$> bLocationP <*> nameP <*> docP <*> maybeSubitems bLayoutsP))
        <|> (WordItemValue <$> vItemP)

wLayoutsP :: Prsr [WordItemParsed]
wLayoutsP = some wLayoutP

mLayoutP :: Prsr MemoryItemParsed
mLayoutP = mwLocationP >>= either mmLayoutP mwLayoutP
    where
        mmLayoutP l = MemoryItemMemory <$> (mkM l <$> nameP <*> docP <*> maybeSubitems mLayoutsP)
        mwLayoutP l = MemoryItemWord   <$> (mkW l <$> nameP <*> docP <*> maybeSubitems wLayoutsP)

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
