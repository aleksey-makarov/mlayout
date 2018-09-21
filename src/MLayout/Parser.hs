{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MLayout.Parser
  ( parser
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List.NonEmpty as LNE hiding (cons, insert)
import           Data.Text hiding (maximum)
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

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
    } deriving Show

data Location s w
  = Location
    { _start        :: StartSet s
    , _width        :: w
    } deriving (Show, Functor, Foldable, Traversable)

wordP :: TokenParsing m => m Word
wordP = fromInteger <$> natural -- FIXME: check boundaries

startArrayP :: (TokenParsing m, Errable m) => m (StartSet (Maybe Word))
startArrayP = do
  start <- optional wordP
  (n, step) <- option (1, Nothing) $ braces $ (,) <$> wordP <*> optional ((char '+') *> wordP)
  return $ StartSetPeriodic start n step

startSetP :: (TokenParsing m, Errable m) => m (StartSet (Maybe Word))
startSetP = StartSet <$> (braces $ sepByNonEmpty ((,) <$> optional wordP <*> nameP) (char ','))

-- FIXME: where StartSet1 constructor?
startP :: (TokenParsing m, Errable m) => m (StartSet (Maybe Word))
startP = startSetP <|> startArrayP

locationP :: (TokenParsing m, Errable m) => m (Location (Maybe Word) (Maybe Word))
locationP =
  mkInterval <$> wordP <*> (char ':' *> wordP) <|>
  flip Location <$> optional wordP <*> (char '@' *> startP) <|>
  mkOneWord <$> optional wordP
    where
      mkInterval :: Word -> Word -> Location (Maybe Word) (Maybe Word)
      mkInterval x y = Location (StartSet1 $ Just a) (Just $ b - a + 1)
        where
          (a, b) = if x > y then (y, x) else (x, y)

      mkOneWord :: Maybe Word -> Location (Maybe Word) (Maybe Word)
      mkOneWord Nothing = Location (StartSet1 Nothing) (Just 1)
      mkOneWord at      = Location (StartSet1 at)      (Nothing)

mlayoutWordWidthP :: (TokenParsing m, Errable m) => m (Location (Maybe Word) (Maybe Word))
mlayoutWordWidthP = do
  w <- mlayoutWordWidthP'
  s <- option (StartSet1 Nothing) (char '@' *> startP)
  return $ Location s (Just w)
    where
      mlayoutWordWidthP' :: TokenParsing m => m Word
      mlayoutWordWidthP' = char '%' *> (1 <$ string "8"  <|>
                                        2 <$ string "16" <|>
                                        4 <$ string "32" <|>
                                        8 <$ string "64")

layoutLocationP :: (TokenParsing m, Errable m) => m (Location (Maybe Word) (Maybe Word))
layoutLocationP = brackets (mlayoutWordWidthP <|> locationP) <?> "layout location"

bitmapLocationP :: (TokenParsing m, Errable m) => m (Location (Maybe Word) (Maybe Word))
bitmapLocationP = angles locationP <?> "bitfield location"

nameP :: (TokenParsing m, Errable m) => m Text
nameP = pack <$> (token $ some $ satisfyRange 'A' 'Z') <?> "name of item"

docP :: (TokenParsing m, Errable m) => m Text
docP = (stringLiteral <|> untilEOLOrBrace) <?> "documentation string"
  where
    untilEOLOrBrace = pack <$> (token $ many $ satisfy (\ c -> c /= '{' && c /= '\n'))

data ValueItem = ValueItem Integer Text Text deriving Show

valueItemP :: (TokenParsing m, Errable m) => m ValueItem
valueItemP = (char '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

type BitmapBody = [Either ValueItem BitmapItem]

bitmapBodyP :: (TokenParsing m, Errable m) => m BitmapBody
bitmapBodyP = some (Left <$> valueItemP <|> Right <$> bitmapItemP)

data BitmapItem = BitmapItem (Location (Maybe Word) (Maybe Word)) Text Text (Maybe BitmapBody) deriving Show

bitmapItemP :: (TokenParsing m, Errable m) => m BitmapItem
bitmapItemP = (BitmapItem <$> bitmapLocationP <*> nameP <*> docP <*> optional (braces bitmapBodyP)) <?> "bitmap item"

type LayoutBody = Either [LayoutItem] BitmapBody

layoutBodyP :: (TokenParsing m, Errable m) => m LayoutBody
layoutBodyP = Left <$> (some layoutItemP) <|> Right <$> bitmapBodyP

data LayoutItem = LayoutItem (Location (Maybe Word) (Maybe Word)) Text Text (Maybe LayoutBody) deriving Show

layoutItemP :: (TokenParsing m, Errable m) => m LayoutItem
layoutItemP = (LayoutItem <$> layoutLocationP <*> nameP <*> docP <*> optional (braces layoutBodyP)) <?> "layout item"

-- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
newtype MLayoutParser a = MLayoutParser { runMLayoutParser :: Parser a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing, CharParsing, LookAheadParsing, Errable)

instance TokenParsing MLayoutParser where
  someSpace = buildSomeSpaceParser (MLayoutParser someSpace) $ CommentStyle "" "" "#" True
-- use the default implementation for other methods:
-- nesting, semi, highlight, token

parser :: Parser [LayoutItem]
parser = runMLayoutParser $ whiteSpace *> (some layoutItemP) <* eof
