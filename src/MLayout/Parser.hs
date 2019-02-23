{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MLayout.Parser
    ( parser
    ) where

import           Prelude as P
import           Control.Applicative
import           Control.Monad
-- import           Data.Aeson
import           Data.Foldable as F
import           Data.HashSet as HS
import           Data.List.NonEmpty as LNE hiding (cons, insert)
import           Data.Text hiding (maximum)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
-- import           Data.Vector as V (fromList, empty)
-- import           Formatting (Format, runFormat, int, stext, (%), sformat)
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
-- type Addr = Word64
-- type Width = Word64
-- type Val = Word64

data StartSet s
    = StartSet
        { _startPositions :: NonEmpty (s, Text)
        }
    | StartSetPeriodic
        { _positionFirst :: s
        , _n             :: Word -- >= 2
        , _step          :: s
        }
    | StartSet1
        { _position :: s
        }
    deriving Show

data ParsedLocation
    = UpTo
        Word -- this is maximum, not upper bound
    | StartWidth
        (StartSet (Maybe Word))
        (Maybe Word)
    deriving Show

throw :: (Applicative m, Errable m) => Format (m b) a -> a
throw f = runFormat f $ raiseErr . failed . TL.unpack . TLB.toLazyText

wordP :: forall a m . (TokenParsing m, Errable m, Monad m, Num a, Integral a, Bounded a) => m a
wordP = do
    v <- natural
    if v < toInteger (minBound :: a) || toInteger (maxBound :: a) < v
        then throw ("should be " % int % " .. " % int) (minBound :: a) (maxBound :: a)
        else return $ fromInteger v

startArrayP :: Prsr (StartSet (Maybe Word))
startArrayP = do
    start <- optional wordP
    (brackets $ StartSetPeriodic start <$> wordP <*> optional ((symbolic '+') *> wordP)) -- FIXME: n should be >= 2
        <|> (return $ StartSet1 start)

startSetP :: Prsr (StartSet (Maybe Word))
startSetP = StartSet <$> (braces $ sepByNonEmpty ((,) <$> optional wordP <*> nameP) (symbolic ','))

startP :: Prsr (StartSet (Maybe Word))
startP = symbolic '@' *> (startSetP <|> startArrayP)

locationP :: Maybe Word -> Prsr ParsedLocation
locationP firstWord  =  mkInterval firstWord <$> (symbolic ':' *> wordP)
                    <|> (flip StartWidth $ firstWord) <$> startP
        where
            mkInterval :: Maybe Word -> Word -> ParsedLocation
            mkInterval (Just x) y = StartWidth (StartSet1 $ Just a) (Just $ b - a + 1)
                where
                    (a, b) = if x > y then (y, x) else (x, y)
            mkInterval Nothing y = UpTo y

layoutLocationInnerP :: Prsr ParsedLocation
layoutLocationInnerP = do
    firstWord <- optional wordP
    locationP firstWord <|> (return $ StartWidth (StartSet1 Nothing) firstWord)

bitmapLocationInnerP :: Prsr ParsedLocation
bitmapLocationInnerP = do
    firstWord <- optional wordP
    locationP firstWord <|> (return $ StartWidth (StartSet1 firstWord) Nothing)

locationWordP :: Prsr ParsedLocation
locationWordP = do
    w <- wordWidthP
    s <- option (StartSet1 Nothing) startP
    return $ StartWidth s (Just w)
        where
            wordWidthP = token (char '%' *> wordWidthDigitsP)
            wordWidthDigitsP  =  1 <$ string "8"
                             <|> 2 <$ string "16"
                             <|> 4 <$ string "32"
                             <|> 8 <$ string "64"

layoutLocationP :: Prsr ParsedLocation
layoutLocationP = brackets (locationWordP <|> layoutLocationInnerP) <?> "layout location"

bitmapLocationP :: Prsr ParsedLocation
bitmapLocationP = angles bitmapLocationInnerP <?> "bitfield location"

nameP :: Prsr Text
nameP = ident (IdentifierStyle "Name Style" upper (alphaNum <|> oneOf "_'") HS.empty Identifier ReservedIdentifier)

docP :: Prsr Text
docP = (stringLiteral <|> untilEOLOrBrace) <?> "documentation string"
    where
        untilEOLOrBrace = pack <$> (token $ many $ satisfy (\ c -> c /= '{' && c /= '\n'))

data ValueItem = ValueItem Integer Text Text deriving Show

valueItemP :: Prsr ValueItem
valueItemP = (symbolic '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

data Item s b
    = Item
        {   _start :: s
        ,   _width :: Word
        ,   _name  :: Text
        ,   _doc   :: Text
        ,   _body  :: Maybe b
        }
    deriving Show

data BitmapBody
    = BitmapBody
        {   _values  :: [ValueItem]
        ,   _bitmaps :: [BitmapItem]
        }
    deriving Show
type BitmapItem = Item (StartSet Word) BitmapBody

data LayoutBody
    = LayoutBody [LayoutItem]
    | LayoutBodyBitmap BitmapBody
    deriving Show
type LayoutItem = Item (StartSet Word) LayoutBody

-- FIXME: use this for itemToList
itemToList :: Item (StartSet Word) b -> [(Word, Word)]
itemToList (Item (StartSet1 s) w _ _ _) = [(s, s + w)]
itemToList (Item (StartSet ss) w _ _ _) = LNE.toList $ fmap f ss
    where
        f (s, _) = (s, s + w)
itemToList (Item (StartSetPeriodic first n step) w _ _ _) = fmap f [0 .. n - 1]
    where
        f n' = let s' = first + n' * step in (s', s' + w)

-- spans :: Item (StartSet Word) b -> Value
-- spans x = Array $ V.fromList $ fmap (String . pairToSpan) (itemToList x)
--     where
--         pairToSpan (a, b) = if b == a + 1 then sformat int a else sformat (int % ":" % int) a (b - 1)

-- instance ToJSON LayoutItem where
--     toJSON x = object [name .= spans x] -- , "children" .= (children $ _body x)]
--         where
--             name = sformat ("[" % stext % "]") (_name x)
--             children (LayoutBody lis) = Array $ V.fromList undefined
--             children (LayoutBodyBitmap lbb) = Array $ V.fromList undefined
--
-- instance ToJSON BitmapItem where
--     toJSON x = object [name .= spans x, "children" .= children]
--         where
--             name = sformat ("<" % stext % ">") (_name x)
--             children :: Value
--             children = Array V.empty

prettyItem :: Pretty b => (Doc a -> Doc a) -> Item (StartSet Word) b -> Doc a
prettyItem envelop (Item s w n d b) = envelop (pretty w <> "@" <> pretty s)
                                   <+> pretty n
                                   <+> dquotes (pretty d)
                                   <+> pretty b

instance Pretty ValueItem where
    pretty (ValueItem v n d) = "=" <> pretty v <+> pretty n <+> dquotes (pretty d)

instance Pretty BitmapItem where
    pretty i = prettyItem PPD.angles i

instance Pretty BitmapBody where
    pretty (BitmapBody vs bms) = PPD.braces (line <> indent 4 (PPD.vsep l) <> line)
        where
            l = fmap pretty vs ++ fmap pretty bms

instance Pretty LayoutBody where
    pretty (LayoutBody lis) = PPD.braces (line <> indent 4 (PPD.vsep (fmap pretty lis)) <> line)
    pretty (LayoutBodyBitmap bb) = pretty bb

instance Pretty (StartSet Word) where
    pretty (StartSet ss) = PPD.braces $ PPD.cat $ punctuate ", " $ LNE.toList $ fmap posPretty ss
        where
            posPretty (at, name) = pretty at <+> pretty name
    pretty (StartSetPeriodic f n s) = pretty f <> PPD.brackets (pretty n <+> "+" <> pretty s)
    pretty (StartSet1 s) = pretty s

instance Pretty LayoutItem where
    pretty i = prettyItem PPD.brackets i

-- FIXME
-- type LayoutTopItem = Item () LayoutBody

maxStartSet :: StartSet Word -> Word
maxStartSet (StartSet ss)            = maximum $ LNE.map fst ss
maxStartSet (StartSetPeriodic f n s) = f + (n - 1) * s
maxStartSet (StartSet1 s)            = s

upperBoundItem :: Item (StartSet Word) b -> Word
upperBoundItem (Item s w _ _ _) = w + maxStartSet s

upperBoundLayoutBody :: LayoutBody -> Word
upperBoundLayoutBody (LayoutBody lb) = upperBoundItemList lb
upperBoundLayoutBody (LayoutBodyBitmap (BitmapBody _ bms)) = (upperBoundItemList bms) `div` 8

upperBoundItemList :: [Item (StartSet Word) b] -> Word
upperBoundItemList = P.foldl f 0
    where
        f x = max x . upperBoundItem

resolve :: [Item (StartSet Word) b] -> Word -> ParsedLocation -> Prsr (StartSet Word, Word)
resolve elderSibs childrenWidth (UpTo maxIndex) = do
    when (widthOfThisItem < childrenWidth) $ throw "width is too small"
    return (StartSet1 upperBoundOfSibs, widthOfThisItem)
    where
        upperBoundOfSibs = upperBoundItemList elderSibs
        widthOfThisItem = maxIndex + 1 - upperBoundOfSibs
resolve elderSibs childrenWidth (StartWidth ss (Nothing)) =
    (, max 1 childrenWidth) <$> resolveParsedLocation elderSibs ss childrenWidth
resolve elderSibs childrenWidth (StartWidth ss (Just widthOfThisItem))  = do
    when (widthOfThisItem < childrenWidth) $ throw ("width is too small @2, widthOfThisItem: " % int % "; childrenWidth: " % int) widthOfThisItem childrenWidth
    (, widthOfThisItem) <$> resolveParsedLocation elderSibs ss widthOfThisItem

-- FIXME: not only for Word
intersectPair :: (Word, Word) -> (Word, Word) -> Bool
intersectPair (l, r) (l', r') = if l < l' then l' < r else l < r'

intersectsList :: (Word, Word) -> [(Word, Word)] -> Bool
intersectsList p = F.any (intersectPair p)

intersectsItems :: (Word, Word) -> [Item (StartSet Word) b] -> Bool
intersectsItems p = intersectsList p . F.concat . fmap itemToList

resolveParsedLocation :: [Item (StartSet Word) b] -> StartSet (Maybe Word) -> Word -> Prsr (StartSet Word)
resolveParsedLocation elderSibs unresolvedStart widthOfThisItem = maybe (throw "intersects") return (resolve' unresolvedStart)
    where
        resolve' :: StartSet (Maybe Word) -> Maybe (StartSet Word)
        resolve' (StartSet1 s)               = StartSet1 . snd <$> resolvePosition upperBoundOfSibs s
        resolve' (StartSet ss)               = StartSet <$> resolveStartSet ss
        resolve' (StartSetPeriodic s n step) = StartSetPeriodic <$> resolvePeriodic <*> Just n <*> Just step'
            where
                step' = maybe widthOfThisItem id step
                -- FIXME: check that *all* intervals do not intersect
                resolvePeriodic = snd <$> resolvePosition upperBoundOfSibs s

        upperBoundOfSibs :: Word
        upperBoundOfSibs = upperBoundItemList elderSibs

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

someFoldlM :: (Alternative m, Monad m) => m b -> (b -> m b) -> m b
someFoldlM first next = first >>= f
    where
        f b' = (optional $ next b') >>= maybe (return b') f

-- FIXME: should be reversed
bitmapBodyFirstP :: Prsr BitmapBody
bitmapBodyFirstP  =  (\ x -> BitmapBody [x] [ ]) <$> valueItemP
                 <|> (\ x -> BitmapBody [ ] [x]) <$> bitmapItemP []

bitmapBodyNextP :: BitmapBody -> Prsr BitmapBody
bitmapBodyNextP (BitmapBody vs bms) =
    do
        v <- valueItemP
        return $ BitmapBody (vs ++ [v]) bms
    <|> do
        bm <- bitmapItemP bms
        return $ BitmapBody vs (bms ++ [bm])

bitmapBodyP :: Prsr BitmapBody
bitmapBodyP = someFoldlM bitmapBodyFirstP bitmapBodyNextP

bitmapItemP :: [Item (StartSet Word) b] -> Prsr BitmapItem
bitmapItemP elderSibs = (do
    l <- bitmapLocationP
    n <- nameP
    d <- docP
    b <- optional (braces bitmapBodyP)
    let bw = maybe 0 (upperBoundItemList . _bitmaps) b
    (s, w) <- resolve elderSibs bw l
    return $ Item s w n d b) <?> "bitmap item"

-- FIXME: should be reversed
layoutBodyXP :: [LayoutItem] -> Prsr [LayoutItem]
layoutBodyXP lis = (\ x -> lis ++ [x]) <$> layoutItemP lis

layoutBodyP :: Prsr LayoutBody
layoutBodyP  =  LayoutBodyBitmap <$> bitmapBodyP
            <|> LayoutBody <$> someFoldlM (layoutBodyXP []) layoutBodyXP

layoutItemP :: [Item (StartSet Word) b] -> Prsr LayoutItem
layoutItemP elderSibs = (do
    l <- layoutLocationP
    n <- nameP
    d <- docP
    b <- optional (braces layoutBodyP)
    let bw = maybe 0 upperBoundLayoutBody b
    (s, w) <- resolve elderSibs bw l
    return $ Item s w n d b) <?> "layout item"

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype Prsr a = Prsr { runPrsr :: Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, Errable)

instance TokenParsing Prsr where
    someSpace = buildSomeSpaceParser (Prsr someSpace) $ CommentStyle "" "" "#" True
-- use the default implementation for other methods:
-- nesting, semi, highlight, token

parser :: Parser [LayoutItem]
parser = runPrsr $ whiteSpace *> (some $ layoutItemP []) <* eof
