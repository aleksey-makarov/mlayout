#!/usr/bin/env stack
{- stack script --resolver nightly-2019-06-21
    --package containers
    --package dhall
    --package directory-tree
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
import Data.Foldable
import Data.Functor.Contravariant
import Data.Text
import Data.Tree
import Dhall as D
import Dhall.Core as DC
import Dhall.Pretty
import Dhall.TypeCheck
import System.Directory.Tree as DirT
import System.Environment

import DhallExtra ()

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

mkDirTree :: FilePath -> IO (Tree DirInfo)
mkDirTree filePath= do
  _ :/ t <- readDirectoryWith f filePath
  case t of
    Failed _ e -> throwIO e
    DirT.File _ _ -> throwIO NotDirException
    Dir n l -> mkTree (pack n) l
    where

      mkTree :: Text -> [DirTree ()] -> IO (Tree DirInfo)
      mkTree n l = do
        (entries, subdirs) <- foldlM ff ([], []) l
        return $ Node (DirInfo n entries) subdirs

      f :: FilePath -> IO ()
      f _ = return ()

      ff :: ([DirEntry], [Tree DirInfo]) -> DirTree () -> IO ([DirEntry], [Tree DirInfo])
      ff (entries, subdirs) = \ case
        Failed _ e -> throwIO e
        DirT.File n _ ->
          let e = case n of
                    'a' : _ -> AFile $ pack n
                    'A' : _ -> AFile $ pack n
                    _ -> OtherFile $ pack n
          in return (e : entries, subdirs)
        Dir n l -> do
          t <- mkTree (pack n) l
          return (entries, t : subdirs)

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
