#!/usr/bin/env stack
{- stack script --resolver nightly-2019-05-29
    --package containers
    --package dhall
    --package directory-tree
    --package recursion-schemes
    --package text
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude as P

import Control.Exception
-- import Control.Monad
import Data.Foldable
import Data.Functor.Contravariant
import Data.Functor.Foldable
import Data.Sequence as DS
import Data.Text
import Data.Tree
import Dhall as D
import Dhall.Core as DC
import Dhall.Map as DM
import Dhall.Parser
import Dhall.TypeCheck
import System.Directory.Tree as DirT
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

--      λ(t : Type)
--    → λ(data : t)
--    → λ(children : List (Tree t))
--    → λ(a : Type)
--    → λ(f : { data : t, subtrees : List a } → a)
--    → f { data = data, subtrees = List/map (Tree t) a (λ(tree : Tree t) → tree a f) children }

mkTree :: Expr Src X -> [Expr Src X] -> Expr Src X
mkTree d children = Lam "t" (Const DC.Type) $
                    Lam "data" (v "t") $
                    Lam "children" (App List (v "Tree")) $
                    Lam "a" (Const DC.Type) $
                    Lam "f" (Pi "_" (Record $ DM.fromList [("data", v "t"), ("subtrees", App List (v "a"))]) (v "a")) $
                    App (v "f") (RecordLit $ DM.fromList [("data", d), ("subtrees", s)])
  where
    v n = Var (V n 0)
    f   = Lam "tree" (App (v "Tree") (v "t")) (App (App (v "tree") (v "a")) (v "f"))
    s   = App (App (App (App (v "List/Map") (App (v "Tree") (v "t"))) (v "a")) f) c'
    c'  = ListLit (Just (App List (App (v "Tree") (v "a")))) (DS.fromList children)

instance Inject d => Inject (Tree d) where
    injectWith options = InputType {..}
      where
        embed (Node d ns) = mkTree (embedIn d) (fmap embed ns)
        declared = undefined
        InputType embedIn declaredIn = injectWith options

-- data TreeF d a = TreeF { rootLabelF :: d, subForestF :: [a] } deriving (Functor, Show)
-- type MuTree d = Mu (TreeF d)

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

  -- parse directory
  t <- getArgs >>= \ case
    n : _ -> mkDirTree n
    _ -> throwIO CmdlineException

  -- test 1
  printTree t

  -- test 2
  let Node d _ = t
  print $ f d
