{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MLayout.Resolver
    ( Location (..)
    , BLayout
    , MLayout
    , resolve
    ) where

import           Control.Exception
import           Data.Functor.Foldable
import           Data.List.NonEmpty
import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Tree

import qualified MLayout.Parser as P
import           MLayout.XTree as XTree
import           MLayout.DataFunctorFoldableExtra

data Location
    = FromTo Word Word                    -- [a:b], a is the first, b is the maximum, not upper bound
    | Fields Word (NonEmpty (Word, Text)) -- [@{A, B, C}], [%12@{12 A, 34 B}] means width and the non-empty list of fields
    | Periodic Word Word Word Word        -- [1@2[3 +4]] means width, start, number of items (>= 2), step
    deriving Show

type BLayout = XTree P.ValueItem (P.Item Location ())

type MLayout = Tree (P.Item Location BLayout)

instance Pretty Location where
    -- FromTo Word Word
    pretty (FromTo from to) = pretty from <> "+" <> pretty to
    -- Fields Word (NonEmpty (Word, Text))
    pretty (Fields w pairs) = pretty w <> "@" <> (braces $ cat $ punctuate ", " $ toList $ fmap posPretty pairs)
        where
            posPretty (mat, name) = pretty mat <+> pretty name
    -- Periodic Word Word Word Word
    pretty (Periodic w start n step) = pretty w <> "@" <> pretty start <> brackets (pretty n <+> "+" <> pretty step)

instance Pretty (P.Item Location ()) where
    pretty (P.Item l n d ()) = angles (pretty l) <+> pretty n <> P.prettyDoc d

instance Pretty (P.Item Location BLayout) where
    pretty (P.Item l n d _) = brackets (pretty l) <+> pretty n <> P.prettyDoc d

instance P.PrettyInternals (P.Item Location BLayout) where
    prettyInternals (P.Item _ _ _ b) = pretty b
    prettyInternalsIsNull (P.Item _ _ _ b) = XTree.null b

data ResolverException = CmdlineException Text deriving (Exception, Show, Eq, Ord)

resolveMLayoutAlg :: (forall a . TreeF (P.Item P.MLocation P.BLayout) (Either ResolverException a) -> Either ResolverException (TreeF (P.Item Location BLayout) a))
resolveMLayoutAlg = undefined

resolveMLayout :: P.MLayout -> Either ResolverException MLayout
resolveMLayout = transverse resolveMLayoutAlg

resolve :: [P.MLayout] -> Either ResolverException [MLayout]
resolve layouts = sequence $ fmap resolveMLayout layouts

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


