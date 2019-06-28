#!/usr/bin/env stack
{- stack script --resolver nightly-2019-06-21
    --package containers
    --package dhall
    --package directory
    --package filepath
    --package recursion-schemes
    --package text
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Prelude as P

import Control.Exception
import Data.Bool as DB
import Data.Either
import Data.Functor.Contravariant
import Data.Text
import Data.Tree
import Dhall as D
import Dhall.Core as DC
import Dhall.Pretty
import Dhall.TypeCheck
import System.Directory
import System.Environment
import System.FilePath

import DhallExtra ()
import DataFunctorFoldableExtra

printTree :: Show a => Tree a -> IO ()
printTree = printTreeOffset 0
  where

    printTreeOffset :: Show a => Word -> Tree a -> IO ()
    printTreeOffset o (Node d f) = do
      let tab = 3
      putStrLn $ (P.map (const ' ') [1 .. tab * o]) ++ (show d)
      mapM_ (printTreeOffset $ o + 1) f

----------------------------------------------------

data DirEntry = AFile Text | OtherFile Text deriving Show
data DirInfo = DirInfo Text [DirEntry] deriving Show

instance Inject DirEntry where
  injectWith _ = adapt >$< inputUnion
    (   (inputConstructor "aFile" :: UnionInputType Text)
    >|< (inputConstructor "otherFile" :: UnionInputType Text)
    )
    where
      adapt (AFile t) = Left t
      adapt (OtherFile t) = Right t

instance Inject DirInfo where
  injectWith _ = inputRecord
    ( adapt >$< inputField "name"
            >*< inputField "dirEntries"
    )
    where
      adapt (DirInfo t es) = (t, es)

----------------------------------------------------

mkDirTreeCoalg :: FilePath -> IO (TreeF DirInfo FilePath)
mkDirTreeCoalg p = do
    (subdirs, files) <- partitionEithers <$> (mapM f =<< listDirectory p)
    return $ TreeF (filesToDirInfo files) ((p </>) <$> subdirs)
        where
            f :: FilePath -> IO (Either FilePath FilePath)
            f p' = DB.bool (Right p') (Left p') <$> doesDirectoryExist pFull
                where
                    pFull = p </> p'

            filesToDirInfo :: [FilePath] -> DirInfo
            -- FIXME: use futumorphism (?) to avoid the usage of takeFileName
            filesToDirInfo fs = DirInfo (pack $ takeFileName p) (filePathToDirEntry <$> fs)

            filePathToDirEntry :: FilePath -> DirEntry
            filePathToDirEntry n@('a' : _) = AFile $ pack n
            filePathToDirEntry n@('A' : _) = AFile $ pack n
            filePathToDirEntry n           = OtherFile $ pack n

mkDirTree :: FilePath -> IO (Tree DirInfo)
mkDirTree = anaM mkDirTreeCoalg

----------------------------------------------------

data NotDirException = NotDirException deriving (Exception, Show, Eq, Ord)
data CmdlineException = CmdlineException deriving (Exception, Show, Eq, Ord)

main :: IO ()
main = do

  -- load dhall functions
  (f :: DirInfo -> Text) <- D.input auto "./TreeTest/formatDirInfo.dhall"
  (f2 :: Tree DirInfo -> Text) <- D.input auto "./TreeTest/formatTree.dhall"

  -- parse directory
  t <- getArgs >>= \ case
    n : _ -> mkDirTree n
    _ -> throwIO CmdlineException

  -- test 1
  printTree t

  -- test 2
  let Node d _ = t
  print $ f d

  -- test 3'
  let tn = Node (2 :: Natural) [Node 3 []]
  let tne = embed (injectWith defaultInterpretOptions) tn
  putStrLn "-------------------------------"
  putStrLn "expression: "
  print $ prettyExpr tne
  putStrLn "-------------------------------"
  putStrLn "its type:"
  print $ prettyExpr <$> (typeOf tne)
  putStrLn "-------------------------------"
  putStrLn "normalized:"
  print $ prettyExpr $ normalize tne

  -- test 3
  let te = embed (injectWith defaultInterpretOptions) t
  putStrLn "-------------------------------"
  putStrLn "directory tree expression: "
  print $ prettyExpr te
  putStrLn "-------------------------------"
  putStrLn "directory tree type:"
  print $ prettyExpr <$> (typeOf te)
  putStrLn "-------------------------------"
  putStrLn "directory tree normalized:"
  print $ prettyExpr $ normalize te

  --test 4
  putStrLn "-------------------------------"
  putStrLn "directory tree formatted with script:"
  putStrLn $ unpack $ f2 t
