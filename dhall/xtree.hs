#!/usr/bin/env stack
{- stack script --resolver nightly-2019-06-21
    --package dhall
    --package directory
    --package filepath
    --package prettyprinter
    --package recursion-schemes
    --package text
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

import Control.Exception
import Control.Monad
import Data.Bool
import Data.Functor.Foldable
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import System.Directory
import System.Environment
import System.FilePath

import XTree

type File = FilePath
type Directory = FilePath
type DirectoryTree = XTree File Directory

-- | A monadic catamorphism
cataM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< mapM c <=< (return . project)

-- | A monadic anamorphism
anaM
  :: (Corecursive t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = (return . embed) <=< mapM a <=< g

mkDirTreeCoalg :: FilePath -> IO (XTreeBase File Directory FilePath)
mkDirTreeCoalg p = XTreeBase <$> (mapM f =<< listDirectory p)
        where
            f :: FilePath -> IO (Either File (Directory, FilePath))
            f p' = bool (Left p') (Right (p', pFull)) <$> doesDirectoryExist pFull
                where
                    pFull = p </> p'

mkDirTree :: FilePath -> IO DirectoryTree
mkDirTree = anaM mkDirTreeCoalg

prettyDirectoryTree :: XTreeBase File Directory (Doc ana) -> Doc ana
prettyDirectoryTree (XTreeBase l) = vcat $ fmap f l
    where
        f :: Either File (Directory, Doc ana) -> Doc ana
        f (Left file) = dquotes (pretty file)
        f (Right (dir, ddoc)) = dquotes (pretty dir) <+> braces (line <> indent 4 ddoc <> line)

instance Pretty DirectoryTree where
    pretty = cata prettyDirectoryTree

data CmdlineException = CmdlineException deriving (Exception, Show, Eq, Ord)

main :: IO ()
main = do

    t <- getArgs >>= \ case
        n : _ -> mkDirTree n
        _ -> throwIO CmdlineException

    putDocW 80 $ (pretty t <> line)
