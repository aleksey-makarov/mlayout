
module MLayout.Parser
  ( parser
  ) where

import           Control.Applicative
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
-- import           Text.Parser.Token.Style
-- import           Text.Trifecta.Parser
import           Text.Trifecta.Result

data LayoutLocation = LayoutLocation Integer deriving Show

layoutLocationP :: (TokenParsing m, Errable m) => m LayoutLocation
layoutLocationP = LayoutLocation <$> brackets integer <?> "layout location"

data BitmapLocation = BitmapLocation Integer deriving Show

bitmapLocationP :: (TokenParsing m, Errable m) => m BitmapLocation
bitmapLocationP = BitmapLocation <$> angles integer <?> "bitfield location"

data Name = Name String deriving Show

nameP :: (TokenParsing m, Errable m) => m Name
nameP = Name <$> (token $ some $ satisfyRange 'A' 'Z') <?> "name of item"

data Doc = Doc String deriving Show

docP :: (TokenParsing m, Errable m) => m Doc
docP = Doc <$> (stringLiteral <|> untilEOLOrBrace) <?> "documentation string"
  where
    untilEOLOrBrace = token $ many $ satisfy (\ c -> c /= '{' && c /= '\n')

data ValueItem = ValueItem Integer Name Doc deriving Show

valueItemP :: (TokenParsing m, Errable m) => m ValueItem
valueItemP = (char '=' *> (ValueItem <$> integer <*> nameP <*> docP)) <?> "value item"

type BitmapBody = [Either ValueItem BitmapItem]

bitmapBodyP :: (TokenParsing m, Errable m) => m BitmapBody
bitmapBodyP = some (Left <$> valueItemP <|> Right <$> bitmapItemP)

data BitmapItem = BitmapItem BitmapLocation Name Doc (Maybe BitmapBody) deriving Show

bitmapItemP :: (TokenParsing m, Errable m) => m BitmapItem
bitmapItemP = (BitmapItem <$> bitmapLocationP <*> nameP <*> docP <*> optional (braces bitmapBodyP)) <?> "bitmap item"

type LayoutBody = Either [LayoutItem] BitmapBody

layoutBodyP :: (TokenParsing m, Errable m) => m LayoutBody
layoutBodyP = Left <$> (some layoutItemP) <|> Right <$> bitmapBodyP

data LayoutItem = LayoutItem LayoutLocation Name Doc (Maybe LayoutBody) deriving Show

layoutItemP :: (TokenParsing m, Errable m) => m LayoutItem
layoutItemP = (LayoutItem <$> layoutLocationP <*> nameP <*> docP <*> optional (braces layoutBodyP)) <?> "layout item"

parser :: (TokenParsing m, Errable m) => m [LayoutItem]
parser = whiteSpace *> (some layoutItemP) <* eof

-- -- | Wrapper around @Text.Parsec.String.Parser@, overriding whitespace lexing.
-- newtype ProtoParser a = ProtoParser { runProtoParser :: Parser a }
--   deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
--            , Parsing, CharParsing, LookAheadParsing)
--
-- instance TokenParsing ProtoParser where
--   someSpace   = TokenStyle.buildSomeSpaceParser
--                   (ProtoParser someSpace)
--                   TokenStyle.javaCommentStyle
-- -- use the default implementation for other methods:
-- -- nesting, semi, highlight, token
