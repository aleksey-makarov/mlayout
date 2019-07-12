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

module MLayout.Parser
    ( MLayout
    , parser
    ) where

import           Prelude as P
import           Control.Applicative
import           Control.Monad
import           Data.Functor.Foldable
import           Data.HashSet as HS
import           Data.List.NonEmpty as LNE hiding (cons, insert)
import           Data.Text hiding (maximum)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Prettyprint.Doc as PPD
import           Data.Text.Prettyprint.Doc hiding (angles, braces, brackets)
import           Data.Tree
import           Formatting (Format, runFormat, int, (%))
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           MLayout.XTree

class Width w where
    fromWord :: Word -> w

data MWidth = W8 | W16 | W32 | W64 | W128 | W Word

instance Width MWidth where
    fromWord = W

instance Width Word where
    fromWord = id

data Location w
    = FromTo (Maybe Word) (Maybe Word)                  -- [a:b], a is the first, b is the maximum, not upper bound
    | WordNext w                                        -- [%32@], [%32], [5@], [5] NB: [2] means [2@], BUT <2> means <@2>
    | Word (Maybe w) Word                               -- [@0x11], [7@0x22], [%16@3]
    | Fields (Maybe w) (NonEmpty (Maybe Word, Text))    -- [@{A, B, C}], [%12@{12 A, 34 B}]
    | Periodic (Maybe w) (Maybe Word) Word (Maybe Word) -- [1@2[3 +4]] means optional width, optional start, mandatory number of items (>= 2), optional step
    deriving Show

type MLocation = Location MWidth -- Word OR W8, W16..
type BLocation = Location Word  -- just Word

data ValueItem = ValueItem Integer Text Text deriving Show   -- = value, name, doc

data Item w d
    = Item
        {   _location :: Location w
        ,   _name  :: Text
        ,   _doc   :: Text -- FIXME: optional?
        ,   _body  :: d
        }
    deriving Show

type BLayout = XTree ValueItem (Item Word ())

type MLayout = Tree (Item MWidth BLayout)

--------------------------------------------------------------------------------

throw :: (Applicative m, Errable m) => Format (m b) a -> a
throw f = runFormat f $ raiseErr . failed . TL.unpack . TLB.toLazyText

wordP :: forall a m . (TokenParsing m, Errable m, Monad m, Num a, Integral a, Bounded a) => m a
wordP = do
    v <- natural
    if v < toInteger (minBound :: a) || toInteger (maxBound :: a) < v
        -- FIXME: in the error message use source number representation
        then throw ("should be " % int % " .. " % int) (minBound :: a) (maxBound :: a)
        else return $ fromInteger v

startArrayP :: Maybe w -> Maybe Word -> Prsr (Location w)
startArrayP maybeWidth maybeStart = Periodic maybeWidth maybeStart <$> wordP <*> optional ((symbolic '+') *> wordP)

-- [..@12[]], [..@12]
startArrayWithPositionP :: Maybe w -> Word -> Prsr (Location w)
startArrayWithPositionP maybeWidth start = brackets (startArrayP maybeWidth (Just start)) <|> return (Word maybeWidth start)

startArrayNoPositionP :: Maybe w -> Prsr (Location w)
startArrayNoPositionP maybeWidth = brackets (startArrayP maybeWidth Nothing)

-- [..@12[34 + 56]] <> [..@12]
startArrayOrSimpleP :: Maybe w -> Prsr (Location w)
startArrayOrSimpleP maybeWidth = startArrayNoPositionP maybeWidth <|> (wordP >>= (startArrayWithPositionP maybeWidth))

startSetP :: Maybe w -> Prsr (Location w)
startSetP firstMaybeWord = Fields firstMaybeWord <$> (braces $ sepByNonEmpty ((,) <$> optional wordP <*> nameP) (symbolic ','))

startP :: Maybe w -> Prsr (Location w)
startP Nothing            = symbolic '@' *> (startSetP Nothing   <|> startArrayOrSimpleP Nothing)
startP justWidth@(Just w) = symbolic '@' *> (startSetP justWidth <|> startArrayOrSimpleP justWidth <|> return (WordNext w))

fromToP :: Maybe Word -> Prsr (Location w)
fromToP maybeFrom = FromTo maybeFrom <$> (symbolic ':' *> optional wordP)

fromToOrStartP :: Width w => Maybe Word -> Prsr (Location w)
fromToOrStartP x = fromToP x <|> startP (fromWord <$> x)

locationP :: Width w => (Word -> Location w) -> Prsr (Location w)
locationP justOneWord = optional wordP >>= maybe (fromToOrStartP Nothing <|> (return $ FromTo Nothing Nothing)) (\x -> fromToOrStartP (Just x) <|> (return $ justOneWord x))

-- <12:12> <|> <12@..>
bLocationP :: Prsr BLocation
bLocationP = angles (locationP (\x -> Word Nothing x)) <?> "bitfield location"

mWidthP :: Prsr MWidth
mWidthP = token (char '%' *> (  W8   <$ string "8"
                            <|> W16  <$ string "16"
                            <|> W32  <$ string "32"
                            <|> W64  <$ string "64"
                            <|> W128 <$ string "128" ))

-- [%12@..], [%12]
mWordP :: Prsr MLocation
mWordP = mWidthP >>= (\x -> startP (Just x) <|> (return (WordNext x)))

-- [mWordP] <|> [:12] <|> [@12] <|> [12:12] <|> [12@..] <|> [12]
mLocationP :: Prsr MLocation
mLocationP = brackets (mWordP <|> (locationP (\x -> WordNext (W x)))) <?> "memory layout location"

valueItemP :: Prsr ValueItem
valueItemP = (symbolic '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

nameP :: Prsr Text
nameP = ident (IdentifierStyle "Name Style" upper (alphaNum <|> oneOf "_'") HS.empty Identifier ReservedIdentifier) <?> "id"

docP :: Prsr Text
docP = stringLiteral <?> "documentation string"

bLayoutItemP :: Prsr (Item Word (), BLayout)
bLayoutItemP = do
    l <- bLocationP
    n <- nameP
    d <- docP
    subitems <- braces bLayoutP <|> (return $ embed $ XTreeF [])
    return (Item l n d (), subitems)

bLayoutP :: Prsr BLayout
bLayoutP = bLayoutP' <?> "bitmap item"
    where
        bLayoutP' = embed . XTreeF <$> many (Left <$> valueItemP <|> Right <$> bLayoutItemP)

mBodyP :: Prsr (BLayout, [MLayout])
mBodyP = braces ((,) <$> bLayoutP <*> some mLayoutP)

mLayoutP :: Prsr MLayout
mLayoutP = mLayoutP' <?> "memory layout item"
    where
        mLayoutP' = do
            l <- mLocationP
            n <- nameP
            d <- docP
            (blayout, subtrees) <- mBodyP
            return $ Node (Item l n d blayout) subtrees

--------------------------------------------------------------------------------

instance Pretty MWidth where
    pretty W8 = "%8"
    pretty W16 = "%16"
    pretty W32 = "%32"
    pretty W64 = "%64"
    pretty W128 = "%128"
    pretty (W w) = pretty w

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
    pretty (Periodic mw mat n ms) = prettyMaybe mw <> "@" <> prettyMaybe mat <> PPD.brackets (pretty n <+> "+" <> prettyMaybe ms)

instance Pretty ValueItem where
    pretty (ValueItem v n d) = "=" <> pretty v <+> pretty n <+> dquotes (pretty d)

xTreeIsEmpty :: XTree l n -> Bool
xTreeIsEmpty (project -> XTreeF []) = True
xTreeIsEmpty _ = False

instance Pretty BLayout where
    pretty (project -> XTreeF ls) = PPD.vsep $ fmap prettyl ls
        where
            prettyl (Left valueItem) = pretty valueItem
            prettyl (Right (Item l n d (), subtree)) = PPD.angles (pretty l)
                                   <+> pretty n
                                   <+> dquotes (pretty d)
                                   <+> if xTreeIsEmpty subtree then mempty else prettyBody subtree
            prettyBody subtree = PPD.braces (line <> indent 4 (pretty subtree) <> line)

instance Pretty MLayout where
    pretty (Node (Item l n d b) s) = PPD.brackets (pretty l)
                                   <+> pretty n
                                   <+> dquotes (pretty d)
                                   <+> if xTreeIsEmpty b then mempty else prettyBody
        where
            prettyBody = PPD.braces (line <> indent 4 ((pretty b) <> line <> PPD.vsep (fmap pretty s)) <> line)

--------------------------------------------------------------------------------

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype Prsr a = Prsr { runPrsr :: Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, Errable)

instance TokenParsing Prsr where
    someSpace = buildSomeSpaceParser (Prsr someSpace) $ CommentStyle "" "" "#" True
-- use the default implementation for other methods:
-- nesting, semi, highlight, token

parser :: Parser [MLayout]
parser = runPrsr $ whiteSpace *> (some $ mLayoutP) <* eof
