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
--    , prettyDoc
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
import           Data.Text.Prettyprint.Doc as PPD
import           Data.Tree
import           Formatting (Format, runFormat, int, (%))
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token as TPT
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

-- import           MLayout.XTree as XTree
-- import           MLayout.DataFunctorFoldableExtra
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

startP :: Maybe Word -> Prsr (LocationParsed)
startP Nothing            = symbolic '@' *> (startSetP Nothing   <|> startArrayOrSimpleP Nothing)
startP justWidth@(Just w) = symbolic '@' *> (startSetP justWidth <|> startArrayOrSimpleP justWidth <|> return (WordNext w))

fromToP :: Maybe Word -> Prsr (LocationParsed)
fromToP maybeFrom = FromTo maybeFrom <$> (symbolic ':' *> optional wordP)

fromToOrStartP :: Maybe Word -> Prsr (LocationParsed)
fromToOrStartP x = fromToP x <|> startP x

locationP :: (Word -> LocationParsed) -> Prsr (LocationParsed)
locationP justOneWord = optional wordP >>= maybe
    (fromToOrStartP Nothing <|> (return $ FromTo Nothing Nothing))
    (\x -> fromToOrStartP (Just x) <|> (return $ justOneWord x))

-- <12:12> <|> <12@..>
bLocationP :: Prsr LocationParsed
bLocationP = TPT.angles (locationP (\x -> Word Nothing x)) <?> "bitfield location"

mWidthP :: Prsr Word
mWidthP = token (char '%' *> (  1  <$ string "8"
                            <|> 2  <$ string "16"
                            <|> 4  <$ string "32"
                            <|> 8  <$ string "64"
                            <|> 16 <$ string "128" ))

-- [%12@..], [%12]
mWordP :: Prsr LocationParsed
mWordP = mWidthP >>= (\x -> startP (Just x) <|> (return (WordNext x)))

-- [mWordP] <|> [:12] <|> [@12] <|> [12:12] <|> [12@..] <|> [12]
wmLocationP :: Prsr (Either LocationParsed LocationParsed) -- left for word, right for memory
wmLocationP = TPT.brackets (Left <$> mWordP <|> Right <$> (locationP (\x -> WordNext x))) <?> "memory or word layout location"

-- valueItemP :: Prsr ValueItem
-- valueItemP = (symbolic '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

nameP :: Prsr Text
nameP = ident (IdentifierStyle "Name Style" upper (alphaNum <|> oneOf "_'") HS.empty Identifier ReservedIdentifier) <?> "id"

docP :: Prsr Text
docP = (stringLiteral <|> return "") <?> "documentation string"

{-

bLayoutItemP :: Prsr (Item BLocation (), BLayout)
bLayoutItemP = do
    l <- bLocationP
    n <- nameP
    d <- docP
    subitems <- TPT.braces bLayoutP <|> return XTree.empty
    return (Item l n d (), subitems)

bLayoutP :: Prsr BLayout
bLayoutP = bLayoutP' <?> "bitmap item"
    where
        bLayoutP' = embed . XTreeF <$> many (Left <$> valueItemP <|> Right <$> bLayoutItemP)

mBodyP :: Prsr (BLayout, [MLayout])
mBodyP = TPT.braces ((,) <$> bLayoutP <*> many mLayoutP)

-}

mLayoutP :: Prsr MLayoutParsed
mLayoutP = mLayoutP' <?> "memory layout item"
    where
        mLayoutP' = undefined
{-
        mLayoutP' = do
            l <- mLocationP
            n <- nameP
            d <- docP
            (blayout, subtrees) <- mBodyP <|> return (XTree.empty, [])
            return $ Node (Item l n d blayout) subtrees
-}
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

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype Prsr a = Prsr { runPrsr :: Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, Errable)

instance TokenParsing Prsr where
    someSpace = buildSomeSpaceParser (Prsr someSpace) $ CommentStyle "" "" "#" True
-- use the default implementation for other methods:
-- nesting, semi, highlight, token

parser :: Parser [MLayoutParsed]
parser = runPrsr $ whiteSpace *> (some $ mLayoutP) <* eof

