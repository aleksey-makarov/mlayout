#!/usr/bin/env stack
{- stack script --resolver nightly-2019-07-12
    --package containers
    --package dhall
    --package directory
    --package filepath
    --package prettyprinter
    --package recursion-schemes
    --package text
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Exception
import Dhall
import Dhall.Pretty
import Dhall.TypeCheck
import Data.Bool
import Data.Functor.Foldable
import Data.Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import System.Directory
import System.Environment
import System.FilePath

import DataFunctorFoldableExtra
import DhallExtra ()
import XTree

type File = Text
type Directory = Text
type DirectoryTree = XTree File Directory

mkDirTreeCoalg :: FilePath -> IO (XTreeF File Directory FilePath)
mkDirTreeCoalg p = XTreeF <$> (mapM f =<< listDirectory p)
        where
            f :: FilePath -> IO (Either File (Directory, FilePath))
            f p' = Data.Bool.bool (Left $ pack p') (Right (pack p', pFull)) <$> doesDirectoryExist pFull
                where
                    pFull = p </> p'

mkDirTree :: FilePath -> IO DirectoryTree
mkDirTree = anaM mkDirTreeCoalg

prettyDirectoryTree :: XTreeF File Directory (Doc ana) -> Doc ana
prettyDirectoryTree (XTreeF l) = vcat $ fmap f l
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

    let directoryTreeInputType = injectWith defaultInterpretOptions
    let te = Dhall.embed directoryTreeInputType t
    putStrLn "-------------------------------"
    putStrLn "expression: "
    print $ prettyExpr te
    putStrLn "-------------------------------"
    putStrLn "its type:"
    either print (print . prettyExpr) (typeOf te)
--    case typeOf te of
--        Left err -> do
--            putStrLn "Error"
--            print err
--        Right tt -> do
--            putStrLn "Ok"
--            print $ prettyExpr tt

    putStrLn "-------------------------------"
    putStrLn "type of its type:"
    either print (print . prettyExpr) (typeOf $ Dhall.declared $ directoryTreeInputType)

    (f :: DirectoryTree -> Text) <- input auto "./XTreeTest.dhall"

    putStrLn "-------------------------------"
    putStrLn "directory tree formatted with script:"
    putStrLn $ unpack $ f t
