{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MLayout.Parser
    ( parser
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.List.NonEmpty as LNE hiding (cons, insert)
import           Data.Text hiding (maximum)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import           Formatting (Format, runFormat, int, (%))
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

-- type Addr = Word64
-- type Width = Word64
-- type Val = Word64

data StartSet s
    = StartSet
        { _startPositions :: NonEmpty (s, Text)
        }
    | StartSetPeriodic
        { _positionFirst :: s
        , _n             :: Word
        , _step          :: s
        }
    | StartSet1
        { _position :: s
        }
    deriving Show

-- data Location s w
--     = Location
--         { _start        :: StartSet s
--         , _width        :: w
--         } deriving (Show, Functor, Foldable, Traversable)

data ParsedLocation
    = UpTo
        { _upTo :: Word
        }
    | StartWidth
        { _start :: StartSet (Maybe Word)
        , _width :: Maybe Word
        }
    deriving Show

throw :: (Applicative m, Errable m) => Format (m b) a -> a
throw m = runFormat m $ raiseErr . failed . TL.unpack . TLB.toLazyText

wordP :: forall a m . (TokenParsing m, Errable m, Monad m, Num a, Integral a, Bounded a) => m a
wordP = do
    v <- natural
    if v < toInteger (minBound :: a) || toInteger (maxBound :: a) < v
        then throw ("should be " % int % " .. " % int) (minBound :: a) (maxBound :: a)
        else return $ fromInteger v

startArrayP :: Prsr (StartSet (Maybe Word))
startArrayP = do
    start <- optional wordP
    (n, step) <- option (1, Nothing) $ braces $ (,) <$> wordP <*> optional ((symbolic '+') *> wordP)
    return $ StartSetPeriodic start n step

startSetP :: Prsr (StartSet (Maybe Word))
startSetP = StartSet <$> (braces $ sepByNonEmpty ((,) <$> optional wordP <*> nameP) (symbolic ','))

startP :: Prsr (StartSet (Maybe Word))
startP = symbolic '@' *> (startSetP <|> startArrayP)

locationP :: Prsr ParsedLocation
locationP = do
    firstWord <- optional wordP
    (mkInterval firstWord <$> (symbolic ':' *> wordP))
        <|> (flip StartWidth $ firstWord) <$> startP
        <|> (return $ mkOneWord firstWord)
        where

            mkInterval :: Maybe Word -> Word -> ParsedLocation
            mkInterval (Just x) y = StartWidth (StartSet1 $ Just a) (Just $ b - a + 1)
                where
                    (a, b) = if x > y then (y, x) else (x, y)
            mkInterval Nothing y = UpTo y

            mkOneWord :: Maybe Word -> ParsedLocation
            mkOneWord Nothing = StartWidth (StartSet1 Nothing) (Just 1)
            mkOneWord at      = StartWidth (StartSet1 at)      (Nothing)

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
layoutLocationP = brackets (locationWordP <|> locationP) <?> "layout location"

bitmapLocationP :: Prsr ParsedLocation
bitmapLocationP = angles locationP <?> "bitfield location"

nameP :: Prsr Text
nameP = pack <$> (token $ some $ satisfyRange 'A' 'Z') <?> "name of item"

docP :: Prsr Text
docP = (stringLiteral <|> untilEOLOrBrace) <?> "documentation string"
    where
        untilEOLOrBrace = pack <$> (token $ many $ satisfy (\ c -> c /= '{' && c /= '\n'))

data ValueItem = ValueItem Integer Text Text deriving Show

valueItemP :: Prsr ValueItem
valueItemP = (char '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

type BitmapBody = [Either ValueItem BitmapItem]

bitmapBodyP :: Prsr BitmapBody
bitmapBodyP = some (Left <$> valueItemP <|> Right <$> bitmapItemP)

data BitmapItem = BitmapItem ParsedLocation Text Text (Maybe BitmapBody) deriving Show

bitmapItemP :: Prsr BitmapItem
bitmapItemP = (BitmapItem <$> bitmapLocationP <*> nameP <*> docP <*> optional (braces bitmapBodyP)) <?> "bitmap item"

type LayoutBody = Either [LayoutItem] BitmapBody

layoutBodyP :: Prsr LayoutBody
layoutBodyP = Left <$> (some layoutItemP) <|> Right <$> bitmapBodyP

data LayoutItem = LayoutItem ParsedLocation Text Text (Maybe LayoutBody) deriving Show

layoutItemP :: Prsr LayoutItem
layoutItemP = (LayoutItem <$> layoutLocationP <*> nameP <*> docP <*> optional (braces layoutBodyP)) <?> "layout item"

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype Prsr a = Prsr { runPrsr :: Parser a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, Errable)

instance TokenParsing Prsr where
    someSpace = buildSomeSpaceParser (Prsr someSpace) $ CommentStyle "" "" "#" True
-- use the default implementation for other methods:
-- nesting, semi, highlight, token

parser :: Parser [LayoutItem]
parser = runPrsr $ whiteSpace *> (some layoutItemP) <* eof
