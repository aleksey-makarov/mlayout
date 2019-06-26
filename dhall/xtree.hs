#!/usr/bin/env stack
{- stack script --resolver nightly-2019-06-21
    --package dhall
    --package directory
    --package prettyprinter
    --package recursion-schemes
    --package text
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Data.Bool
import Data.Functor.Foldable
import Data.Text
import Data.Text.Prettyprint.Doc
import System.Directory

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
            f p = bool (Right (p, p)) (Left p) <$> doesDirectoryExist p

mkDirTree :: FilePath -> IO DirectoryTree
mkDirTree = anaM mkDirTreeCoalg

prettyDirectoryTree :: XTreeBase File Directory (Doc ana) -> Doc ana
prettyDirectoryTree = undefined

instance Pretty DirectoryTree where
    pretty = cata prettyDirectoryTree

main = do
    putStrLn "Hello world"
