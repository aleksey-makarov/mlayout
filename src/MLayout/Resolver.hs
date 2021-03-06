{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wall #-}

module MLayout.Resolver
    ( resolve
    , ResolverException (..)
    ) where

import           Control.Exception
-- import           Control.Monad
-- import           Control.Monad.Trans.State
-- import           Data.List.NonEmpty
import           Data.Text
import           Prelude as P

import           MLayout.Data
-- import           MLayout.HFunctor

data ResolverException = ResolverException Text deriving (Exception, Show, Eq, Ord)

resolve :: [MemoryItemParsed] -> Either ResolverException [MemoryItemResolved]
resolve _ = Left $ ResolverException "Not implemented"

{-

x :: (Maybe Word) -> Word
x = maybe 0 id

ww :: WordWidth -> Word
ww W8   = 1
ww W16  = 2
ww W32  = 4
ww W64  = 8
ww W128 = 16

resolvePositions :: NonEmpty (Maybe Word, Text) -> NonEmpty (Word, Text)
resolvePositions = fmap f
    where
        f (mAt, name) = (x mAt, name)

type S = Word
type M = StateT S (Either ResolverException)

initialState :: S
initialState = 0

upperBound :: LocationResolved -> Word
upperBound (ResolvedWidthStart w s) = s + w
upperBound (ResolvedFields w ss) = (P.maximum $ fmap fst ss) + w
upperBound (ResolvedPeriodic _ s n step) = s + n * step

class ResolvableLocation x where
    resolveLocation :: Word -> x -> M (x, LocationResolved)

instance ResolvableLocation LocationMB where
    resolveLocation _ p@(FromTo mFrom _)                           = return (p, ResolvedWidthStart 0 (x mFrom))
    resolveLocation _ p@(WidthStart mWidth Next)                   = return (p, ResolvedWidthStart (x mWidth) 0)
    resolveLocation _ p@(WidthStart mWidth (Simple at))            = return (p, ResolvedWidthStart (x mWidth) at)
    resolveLocation _ p@(WidthStart mWidth (Fields positions))     = return (p, ResolvedFields (x mWidth) (resolvePositions positions))
    resolveLocation _ p@(WidthStart mWidth (Periodic mAt n mStep)) = return (p, ResolvedPeriodic (x mWidth) (x mAt) n (x mStep))

instance ResolvableLocation LocationW where
    resolveLocation _ p@(LocationParsedWord w Next)                   = return (p, ResolvedWidthStart (ww w) 0)
    resolveLocation _ p@(LocationParsedWord w (Simple at))            = return (p, ResolvedWidthStart (ww w) at)
    resolveLocation _ p@(LocationParsedWord w (Fields positions))     = return (p, ResolvedFields (ww w) (resolvePositions positions))
    resolveLocation _ p@(LocationParsedWord w (Periodic mAt n mStep)) = return (p, ResolvedPeriodic (ww w) (x mAt) n (x mStep))

newtype R a = R { unR :: M (HFix (MLayoutF Resolved) a) }

unRsubitems :: HTraversable' h => [h R] -> M [h (HFix (MLayoutF Resolved))]
unRsubitems is = sequence $ fmap (htraverse' unR) is

resolveAlg' :: forall a . (BuildableLayout a, HTraversable' (ItemType a), ResolvableLocation (Location Parsed a)) => Location Parsed a -> Text -> Text -> [ ItemType a R ] -> R a
resolveAlg' l n d xis = R $ do
    start <- get
    subitems <- unRsubitems xis
    resolvedLocation@(_, r) <- resolveLocation start l
    start' <- get
    when (upperBound r < start') $ error "wrong width" -- FIXME
    put $ upperBound r
    return $ mk resolvedLocation n d subitems

resolveAlg :: MLayoutF Parsed R :~> R
resolveAlg (MLayoutMemoryF l n d mis) = resolveAlg' l n d mis
resolveAlg (MLayoutWordF   l n d wis) = resolveAlg' l n d wis
resolveAlg (MLayoutBitsF   l n d bis) = resolveAlg' l n d bis

resolve1 :: MemoryItemParsed -> Either ResolverException MemoryItemResolved
resolve1 (MemoryItemMemory m) = MemoryItemMemory <$> evalStateT (unR $ hcata resolveAlg m) initialState
resolve1 (MemoryItemWord m)   = MemoryItemWord   <$> evalStateT (unR $ hcata resolveAlg m) initialState

resolve :: [MemoryItemParsed] -> Either ResolverException [MemoryItemResolved]
resolve layouts = sequence $ fmap resolve1 layouts

{ -
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


