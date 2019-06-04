#!/usr/bin/env stack
{- stack script --resolver nightly-2019-05-29
    --package containers
    --package dhall
    --package directory-tree
    --package recursion-schemes
    --package text
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude as P

import Control.Exception
-- import Control.Monad
import Data.Foldable
import Data.Functor.Contravariant
import Data.Text
import Data.Tree
import Dhall
import System.Directory.Tree
import System.Environment

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

injectDirEntry :: InputType DirEntry
injectDirEntry = adapt >$< inputUnion
  (   (inputConstructor "aFile" :: UnionInputType Text)
  >|< (inputConstructor "otherFile" :: UnionInputType Text)
  )
  where
    adapt (AFile t) = Left t
    adapt (OtherFile t) = Right t

instance Inject DirEntry where
  injectWith _ = injectDirEntry

data DirInfo = DirInfo Text [DirEntry] deriving Show

injectDirInfo :: InputType DirInfo
injectDirInfo =
  inputRecord
    ( adapt >$< inputField "name"
            >*< inputField "dirEntries"
    )
  where
    adapt (DirInfo t es) = (t, es)

instance Inject DirInfo where
  injectWith _ = injectDirInfo

----------------------------------------------------

mkDirTree :: FilePath -> IO (Tree DirInfo)
mkDirTree filePath= do
  _ :/ t <- readDirectoryWith f filePath
  case t of
    Failed _ e -> throwIO e
    File _ _ -> throwIO NotDirException
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
        File n _ ->
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
  (f :: DirInfo -> Text) <- input auto "./TreeTest/formatDirInfo.dhall"

  -- parse directory
  t <- getArgs >>= \ case
    n : _ -> mkDirTree n
    _ -> throwIO CmdlineException

  -- test 1
  printTree t

  -- test 2
  let Node d _ = t
  print $ f d
