#!/usr/bin/env stack
{- stack script --resolver nightly-2019-05-29
    --package directory-tree
    --package recursion-schemes
    --package text
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude as P

import Control.Exception
-- import Control.Monad
import Data.Foldable
import Data.Text
import System.Directory.Tree
import System.Environment

import Tree

printTree :: Show a => Tree a -> IO ()
printTree = printTreeOffset 0
  where
    printTreeOffset :: Show a => Word -> Tree a -> IO ()
    printTreeOffset o t = do
      let tab = 3
      putStrLn $ (P.map (const ' ') [1 .. tab * o]) ++ (show $ treeData t)
      mapM_ (printTreeOffset $ o + 1) (treeSubtrees t)

myTree :: Tree Word
myTree =
  tree 0 [tree 1 [], tree 2 [tree 3 [], tree 4 []], tree 5 []]

data DirEntry = AFile Text | OtherFile Text deriving Show
data DirInfo = DirInfo Text [DirEntry] deriving Show

data NotDirException = NotDirException deriving (Exception, Show, Eq, Ord)
data CmdlineException = CmdlineException deriving (Exception, Show, Eq, Ord)

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
        return $ tree (DirInfo n entries) subdirs
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

main :: IO ()
main = do
  printTree myTree
  getArgs >>= \ case
    n : _ -> do
      t <- mkDirTree n
      printTree t
    _ -> throwIO CmdlineException
