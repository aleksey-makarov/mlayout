#!/usr/bin/env stack
{- stack script --resolver nightly-2019-06-21
    --package dhall
    --package recursion-schemes
    --package text
-}

import Data.Text

import XTree

type File = Text
type Directory = Text
type DirectoryTree = XTree File Directory

mkDirTree :: FilePath -> IO DirectoryTree
mkDirTree = undefined

main = do
    putStrLn "Hello world"
