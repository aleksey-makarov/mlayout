{-# LANGUAGE ApplicativeDo #-}
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

startArrayP :: Maybe Word -> Maybe Word -> Prsr (LocationParsed)
startArrayP maybeWidth maybeStart = Periodic maybeWidth maybeStart <$> wordP <*> optional ((symbolic '+') *> wordP)

-- [..@12[]], [..@12]
startArrayWithPositionP :: Maybe Word -> Word -> Prsr (LocationParsed)
startArrayWithPositionP maybeWidth start = TPT.brackets (startArrayP maybeWidth (Just start)) <|> return (Word maybeWidth start)

startArrayNoPositionP :: Maybe Word -> Prsr (LocationParsed)
startArrayNoPositionP maybeWidth = TPT.brackets (startArrayP maybeWidth Nothing)

-- [..@12[34 + 56]] <> [..@12]
startArrayOrSimpleP :: Maybe Word -> Prsr (LocationParsed)
startArrayOrSimpleP maybeWidth = startArrayNoPositionP maybeWidth <|> (wordP >>= (startArrayWithPositionP maybeWidth))

startSetP :: Maybe Word -> Prsr (LocationParsed)
startSetP firstMaybeWord = Fields firstMaybeWord <$> (TPT.braces $ sepByNonEmpty ((,) <$> optional wordP <*> nameP) (symbolic ','))

atP :: Maybe Word -> Prsr (LocationParsed)
atP Nothing            = symbolic '@' *> (startSetP Nothing   <|> startArrayOrSimpleP Nothing)
atP justWidth@(Just w) = symbolic '@' *> (startSetP justWidth <|> startArrayOrSimpleP justWidth <|> return (WordNext w))

fromToP :: Maybe Word -> Prsr (LocationParsed)
fromToP maybeFrom = FromTo maybeFrom <$> (symbolic ':' *> optional wordP)

fromToOrAtP :: Maybe Word -> Prsr (LocationParsed)
fromToOrAtP x = fromToP x <|> atP x

justOneWordLocationBits :: Word -> LocationParsed
justOneWordLocationBits w = Word Nothing w

justOneWordLocationMemory :: Word -> LocationParsed
justOneWordLocationMemory w = WordNext w

locationInternalsP :: (Word -> LocationParsed) -> Prsr (LocationParsed)
locationInternalsP justOneWord = optional wordP >>= maybe
    (fromToOrAtP Nothing <|> (return $ FromTo Nothing Nothing))
    (\x -> fromToOrAtP (Just x) <|> (return $ justOneWord x))

wWidthP :: Prsr Word
wWidthP = token (char '%' *> (  1  <$ string "8"
                            <|> 2  <$ string "16"
                            <|> 4  <$ string "32"
                            <|> 8  <$ string "64"
                            <|> 16 <$ string "128" ))

-- [%12@..], [%12]
wLocationInternalsP :: Prsr LocationParsed
wLocationInternalsP = wWidthP >>= (\x -> atP (Just x) <|> (return (WordNext x)))

bLocationInternalsP :: Prsr LocationParsed
bLocationInternalsP = locationInternalsP justOneWordLocationBits

mLocationInternalsP :: Prsr LocationParsed
mLocationInternalsP = locationInternalsP justOneWordLocationMemory

-- <12:12> <|> <12@..>
bLocationP :: Prsr LocationParsed
bLocationP = TPT.angles bLocationInternalsP

-- [mWordP] <|> [:12] <|> [@12] <|> [12:12] <|> [12@..] <|> [12]
mwLocationP :: Prsr (Either LocationParsed LocationParsed) -- left for memory, right for word
mwLocationP = TPT.brackets (Right <$> wLocationInternalsP <|> Left <$> mLocationInternalsP)

wLocationP :: Prsr LocationParsed
wLocationP = TPT.brackets wLocationInternalsP

nameP :: Prsr Text
nameP = ident (IdentifierStyle "Name Style" upper (alphaNum <|> oneOf "_'") HS.empty Identifier ReservedIdentifier) <?> "id"

docP :: Prsr Text
docP = (stringLiteral <|> return "") <?> "documentation string"

itemTailP :: LocationParsed -> Prsr (ItemDescription LocationParsed)
itemTailP l = ItemDescription l <$> nameP <*> docP

mwItemP :: Prsr (Either (ItemDescription LocationParsed) (ItemDescription LocationParsed)) -- Left for mem, Right for word
mwItemP = mwLocationP >>= either mItemTailP wItemTailP
    where
        mItemTailP l = Left  <$> itemTailP l
        wItemTailP l = Right <$> itemTailP l

wbItemP :: Prsr (Either (ItemDescription LocationParsed) (ItemDescription LocationParsed)) -- Left for word, Right for bits
wbItemP = (Left <$> (wLocationP >>= itemTailP)) <|> (Right <$> (bLocationP >>= itemTailP))

bItemP :: Prsr (ItemDescription LocationParsed)
bItemP = bLocationP >>= itemTailP

vItemP :: Prsr ValueItem
vItemP = (symbolic '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

bLayoutP :: Prsr BitsItemParsed
bLayoutP = (BitsItemBits <$> (mkB <$> bItemP <*> bLayoutsP)) <|> (BitsItemValue <$> vItemP)

bLayoutsP :: Prsr [BitsItemParsed]
bLayoutsP = some bLayoutP

wLayoutP :: Prsr WordItemParsed
wLayoutP = (wbItemP >>= either wwLayoutP wbLayoutP) <|> (WordItemValue <$> vItemP)
    where
        wwLayoutP d = WordItemWord . mkW d <$> wLayoutsP
        wbLayoutP d = WordItemBits . mkB d <$> bLayoutsP

wLayoutsP :: Prsr [WordItemParsed]
wLayoutsP = some wLayoutP

mLayoutP :: Prsr MemoryItemParsed
mLayoutP = mwItemP >>= either mmLayoutP mwLayoutP
    where
        mmLayoutP d = MemoryItemMemory . mkM d <$> mLayoutsP
        mwLayoutP d = MemoryItemWord   . mkW d <$> wLayoutsP

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
