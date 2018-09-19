#!/usr/bin/env stack
-- stack --resolver nightly-2018-06-13 script --package trifecta --package parsers
{-# OPTIONS_GHC -Wall #-}

import           Control.Applicative
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

data LayoutLocation = LayoutLocation String deriving Show

layoutLocationP :: (TokenParsing m, Errable m) => m LayoutLocation
layoutLocationP = (LayoutLocation <$> brackets (some digit)) <?> "layout location"

data BitmapLocation = BitmapLocation String deriving Show

bitmapLocationP :: (TokenParsing m, Errable m) => m BitmapLocation
bitmapLocationP = (BitmapLocation <$> angles (some digit)) <?> "bitfield location"

data Name = Name String deriving Show

nameP :: (TokenParsing m, Errable m) => m Name
nameP = Name <$> (some $ satisfyRange 'A' 'Z') <?> "name of item"

data Doc = Doc String deriving Show

docP :: (TokenParsing m, Errable m) => m Doc
docP = Doc <$> (stringLiteral <|> untilEOForBrace) <?> "documentation string"
  where
    untilEOForBrace = (runUnspaced $ many $ satisfy (\ c -> c /= '{' && c /= '\n'))

data ValueItem = ValueItem String Name Doc deriving Show

numberP :: (TokenParsing m, Errable m) => m String
numberP = some digit <?> "number"

valueItem :: (TokenParsing m, Errable m) => m ValueItem
valueItem = (char '=' *> (ValueItem <$> numberP <*> nameP <*> docP)) <?> "value item"

type BitmapBody = [Either ValueItem BitmapItem]

bitmapBodyP :: (TokenParsing m, Errable m) => m BitmapBody
bitmapBodyP = undefined

data BitmapItem = BitmapItem BitmapLocation Name Doc (Maybe BitmapBody) deriving Show

bitmapItem :: (TokenParsing m, Errable m) => m BitmapItem
bitmapItem = (BitmapItem <$> bitmapLocationP <*> nameP <*> docP <*> optional (braces bitmapBodyP)) <?> "bitmap item"

type LayoutBody = Either [LayoutItem] BitmapBody

layoutBodyP :: (TokenParsing m, Errable m) => m LayoutBody
layoutBodyP = undefined

data LayoutItem = LayoutItem LayoutLocation Name Doc (Maybe LayoutBody) deriving Show

layoutItem :: (TokenParsing m, Errable m) => m LayoutItem
layoutItem = (LayoutItem <$> layoutLocationP <*> nameP <*> docP <*> optional (braces layoutBodyP)) <?> "layout item"

parser :: (TokenParsing m, Errable m) => m [LayoutItem]
parser = whiteSpace *> (some layoutItem) <* eof

main :: IO ()
main = do
  result <- parseFromFile parser "trifecta.txt"
  case result of
    Nothing -> putStrLn "Failure"
    Just x  -> do
      putStrLn "Success"
      print x
