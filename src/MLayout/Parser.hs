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

module MLayout.Parser
    ( MLayout
    , parser
    ) where

import           Prelude as P
import           Control.Applicative
import           Control.Monad
import           Data.Either
import           Data.HashSet as HS
import           Data.List.NonEmpty as LNE hiding (cons, insert)
import           Data.Text hiding (maximum)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
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
import qualified Data.Text.Prettyprint.Doc as PPD
import           Data.Text.Prettyprint.Doc hiding (angles, braces, brackets)

data Width = W8 | W16 | W32 | W64 | W128 | W Word

data Location w
    = FromTo (Maybe Word) (Maybe Word)                  -- [a:b], a is the first, b is the maximum, not upper bound
    | WordNext w                                        -- [%32@], [%32], [5@], [5] NB: [2] means [2@], BUT <2> means <@2>
    | Word (Maybe w) Word                               -- [@0x11], [7@0x22], [%16@3]
    | Fields (Maybe w) (NonEmpty (Maybe Word, Text))    -- [@{A, B, C}], [%12@{12 A, 34 B}]
    | Periodic (Maybe w) (Maybe Word) Word (Maybe Word) -- [1@2[3 +4]] means optional width, optional start, mandatory number of items (>= 2), optional step
    deriving Show

type MLocation = Location Width -- Word OR W8, W16..
type BLocation = Location Word  -- just Word

data ValueItem = ValueItem Integer Text Text deriving Show   -- = value, name, doc

data Item w d
    = Item
        {   _location :: Location w
        ,   _name  :: Text
        ,   _doc   :: Text
        ,   _body  :: d
        }
    deriving Show

type BData = [ValueItem]

type BLayout = Tree (Item Word BData)

type MData = [BLayout]

type MLayout = Tree (Item Width MData)

--------------------------------------------------------------------------------

throw :: (Applicative m, Errable m) => Format (m b) a -> a
throw f = runFormat f $ raiseErr . failed . TL.unpack . TLB.toLazyText

wordP :: forall a m . (TokenParsing m, Errable m, Monad m, Num a, Integral a, Bounded a) => m a
wordP = do
    v <- natural
    if v < toInteger (minBound :: a) || toInteger (maxBound :: a) < v
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

toWidth :: Word -> w
toWidth = undefined

fromToOrStartP :: Maybe Word -> Prsr (Location w)
fromToOrStartP x = fromToP x <|> startP (toWidth <$> x)

locationP :: (Word -> Location w) -> Prsr (Location w)
locationP justOneWord = optional wordP >>= maybe (fromToOrStartP Nothing) (\x -> fromToOrStartP (Just x) <|> (return $ justOneWord x))

-- <12:12> <|> <12@..>
bLocationP :: Prsr BLocation
bLocationP = angles (locationP (\x -> Word Nothing x)) <?> "bitfield location"

mWidthP :: Prsr Width
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

bBodyP :: Prsr (BData, [BLayout])
bBodyP = braces bBodyP' <|> return ([], [])
    where
        bBodyP' = partitionEithers <$> some (Left <$> valueItemP <|> Right <$> bLayoutP)

bLayoutP :: Prsr BLayout
bLayoutP = bLayoutP' <?> "bitmap item"
    where
        bLayoutP' = do
            l <- bLocationP
            n <- nameP
            d <- docP
            (bdata, subtrees) <- bBodyP
            return $ Node (Item l n d bdata) subtrees

mBodyP :: Prsr (MData, [MLayout])
mBodyP = braces mBodyP' <|> return ([], [])
    where
        mBodyP' = partitionEithers <$> some (Left <$> bLayoutP <|> Right <$> mLayoutP)

mLayoutP :: Prsr MLayout
mLayoutP = mLayoutP' <?> "memory layout item"
    where
        mLayoutP' = do
            l <- mLocationP
            n <- nameP
            d <- docP
            (mdata, subtrees) <- mBodyP
            return $ Node (Item l n d mdata) subtrees

--------------------------------------------------------------------------------

instance Pretty Width where
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

prettyItem :: (Pretty w, Pretty (Tree (Item w d))) => (Doc ann -> Doc ann) -> [Doc ann] -> Tree (Item w d) -> Doc ann
prettyItem envelop prettyDataList (Node (Item l n d _) s) = envelop (envelop $ pretty l)
                                   <+> pretty n
                                   <+> dquotes (pretty d)
                                   <+> if bodyIsEmpty then mempty else prettyBody
    where
        bodyIsEmpty = case (prettyDataList, s) of ([], []) -> True ; _ -> False
        prettyBody = PPD.braces (line <> indent 4 (PPD.vsep (prettyDataList ++ fmap pretty s)) <> line)


instance Pretty BLayout where
    pretty i@(Node (Item _ _ _ b) _) = prettyItem PPD.angles (fmap pretty b) i

instance Pretty MLayout where
    pretty i@(Node (Item _ _ _ b) _) = prettyItem PPD.brackets (fmap pretty b) i

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

{-
-- FIXME: use this for itemToList
itemToList :: Item b -> [(Word, Word)]
itemToList (Item w (StartSet1 s) _ _ _) = [(s, s + w)]
itemToList (Item w (StartSet ss) _ _ _) = LNE.toList $ fmap f ss
    where
        f (s, _) = (s, s + w)
itemToList (Item w (StartSetPeriodic first n step) _ _ _) = fmap f [0 .. n - 1]
    where
        f n' = let s' = first + n' * step in (s', s' + w)

bitmapBodyIsEmpty :: BitmapBody -> Bool
bitmapBodyIsEmpty (BitmapBody [] []) = True
bitmapBodyIsEmpty _ = False

layoutBodyIsEmpty :: LayoutBody -> Bool
layoutBodyIsEmpty (LayoutBody []) = True
layoutBodyIsEmpty (LayoutBodyBitmap x) = bitmapBodyIsEmpty x
layoutBodyIsEmpty _ = False


-- FIXME
-- type LayoutTopItem = Item () LayoutBody

maxStartSet :: StartSet -> Word
maxStartSet (StartSet ss)            = maximum $ LNE.map fst ss
maxStartSet (StartSetPeriodic f n s) = f + (n - 1) * s
maxStartSet (StartSet1 s)            = s

upperBoundItem :: Item b -> Word
upperBoundItem (Item w s _ _ _) = w + maxStartSet s

upperBoundLayoutBody :: LayoutBody -> Word
upperBoundLayoutBody (LayoutBody lb) = upperBoundItemList lb
upperBoundLayoutBody (LayoutBodyBitmap (BitmapBody _ bms)) = (upperBoundItemList bms) `div` 8

upperBoundItemList :: [Item b] -> Word
upperBoundItemList = P.foldl f 0
    where
        f x = max x . upperBoundItem
-}

{-
resolve :: [Item b] -> Word -> Location -> Prsr (Word, StartSet)
resolve elderSibs childrenWidth = resolve'
    where
        upperBoundOfSibs :: Word
        upperBoundOfSibs = upperBoundItemList elderSibs

        resolve' :: Location -> Prsr (Word, StartSet)
        resolve' (FromTo (Just x) y) = (w, ) <$> resolveW w (StartSet1 $ Just a)
            where
                (a, b) = if x > y then (y, x) else (x, y)
                w = b - a + 1
        resolve' (FromTo Nothing to) = do
            when (w < childrenWidth) $ throw "width is too small"
            return (w, StartSet1 upperBoundOfSibs)
            where
                w = to + 1 - upperBoundOfSibs
        resolve' (Word w ss) = (w, ) <$> resolveW w ss
        resolve' (WidthStart (Nothing) ss) = (max 1 childrenWidth, ) <$> resolveW childrenWidth ss
        resolve' (WidthStart (Just w) ss) = do
            when (w < childrenWidth) $ throw ("width is too small @2, w: " % int % "; childrenWidth: " % int) w childrenWidth
            (w, ) <$> resolveW w ss

        resolveW :: Word -> ParsedStartSet -> Prsr StartSet
        resolveW widthOfThisItem unresolvedStart = maybe (throw "intersects") return (resolve'' unresolvedStart)
            where
                resolve'' :: ParsedStartSet -> Maybe StartSet
                resolve'' (StartSet1 s)               = StartSet1 . snd <$> resolvePosition upperBoundOfSibs s
                resolve'' (StartSet ss)               = StartSet <$> resolveStartSet ss
                resolve'' (StartSetPeriodic s n step) = StartSetPeriodic <$> resolvePeriodic <*> Just n <*> Just step'
                    where
                        step' = maybe widthOfThisItem id step
                        -- FIXME: check that *all* intervals do not intersect
                        resolvePeriodic = snd <$> resolvePosition upperBoundOfSibs s

                -- | Resolves the start of the layout
                resolvePosition :: Word               -- ^ First available position
                                -> Maybe Word         -- ^ Where the layout should start, if specified
                                -> Maybe (Word, Word) -- ^ (The new first available position, the resolved start)
                resolvePosition firstAvailable Nothing   = Just (firstAvailable + widthOfThisItem, firstAvailable)
                resolvePosition firstAvailable (Just s) = if s < firstAvailable
                    then if intersectsItems (s, s + widthOfThisItem) elderSibs
                        then Nothing
                        else Just (firstAvailable, s)
                    else Just (s + widthOfThisItem, s)

                resolveStartSet :: NonEmpty (Maybe Word, Text) -> Maybe (NonEmpty (Word, Text))
                -- FIXME: absolutely incorrect: wrong order
                resolveStartSet ((s, n) :| sns) = do
                    (at', s') <- resolvePosition upperBoundOfSibs s
                    ((s', n) :| ) . snd <$> F.foldr f (Just (at', [])) sns

                f :: (Maybe Word, Text) -> Maybe (Word, [(Word, Text)]) -> Maybe (Word, [(Word, Text)])
                f (s, n) state = do
                    (at, sns) <- state
                    (at', s') <- resolvePosition at s
                    checkPrev (P.map fst sns) s'
                    return (at', (s', n) : sns)

                checkPrev :: [Word] -> Word -> Maybe ()
                checkPrev sns s = if intersectsList (s, s + widthOfThisItem) (P.map (\ x -> (x, x + widthOfThisItem)) sns)
                    then Nothing
                    else Just ()

-- FIXME: not only for Word
intersectPair :: (Word, Word) -> (Word, Word) -> Bool
intersectPair (l, r) (l', r') = if l < l' then l' < r else l < r'

intersectsList :: (Word, Word) -> [(Word, Word)] -> Bool
intersectsList p = F.any (intersectPair p)

-- intersectsItems :: (Word, Word) -> [Item b] -> Bool
-- intersectsItems p = intersectsList p . F.concat . fmap itemToList

someFoldlM :: (Alternative m, Monad m) => m b -> (b -> m b) -> m b
someFoldlM first next = first >>= f
    where
        f b' = (optional $ next b') >>= maybe (return b') f

-}


