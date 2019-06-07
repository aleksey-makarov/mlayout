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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude as P

import Control.Exception
-- import Control.Monad
import Data.Foldable
import Data.Functor.Contravariant
-- import Data.Functor.Foldable
import Data.List.NonEmpty as LNE
import Data.Sequence as DS
import Data.Text
import Data.Tree
import Dhall as D
import Dhall.Core as DC
import Dhall.Map as DM
import Dhall.Parser
import Dhall.Pretty
import Dhall.TH
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

--    → λ(a : Type)
--    → λ(f : { data : dtype, subtrees : List a } → a)
--    → f { data = d, subtrees = List/map (Tree dtype) a (λ(tree : Tree dtype) → tree a f) children }

-- TODO: rewrite App as infix

treeToDhall :: Expr Src X -> Expr Src X -> [Expr Src X] -> Expr Src X
treeToDhall dtype d children =
                    Lam "a" (Const DC.Type) $
                    Lam "f" (Pi "_" (Record $ DM.fromList [("data", dtype), ("subtrees", App List (v "a"))]) (v "a")) $
                    App (v "f") (RecordLit $ DM.fromList [("data", d), ("subtrees", s)])
  where
    v n = Var (V n 0)
    f   = Lam "tree" (App (v "Tree") dtype) (App (App (v "tree") (v "a")) (v "f"))
    s   = App (App (App (App (v "List/map") (App (v "Tree") dtype)) (v "a")) f) c'
    c'  = ListLit (Just (App (v "Tree") dtype)) (DS.fromList children)

appendLets :: Expr Src X -> Expr Src X
appendLets e = Let (LNE.fromList [Binding "Tree" Nothing treeType, Binding "List/map" Nothing listMap]) e
  where
    treeType = $(staticDhallExpression "./Tree/Type")
    listMap = $(staticDhallExpression "./List/map")

instance Inject d => Inject (Tree d) where
    injectWith options = InputType {..}
      where
        -- FIXME: add normalize
        embed t = appendLets $ embed' t
        embed' (Node d ns) = treeToDhall declaredIn (embedIn d) (fmap embed' ns)
        -- ∀(a : Type) → ({ data : t, subtrees : List a } -> a) -> a
        declared = Pi "a" (Const DC.Type) $ Pi "_" (Pi "_" (Record $ DM.fromList [("data", declaredIn), ("subtrees", (App List (v "a")))]) (v "a")) (v "a")
        InputType embedIn declaredIn = injectWith options
        v n = Var (V n 0)

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
