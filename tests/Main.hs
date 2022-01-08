{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Monad
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory
import System.FilePath.Posix
import System.IO
import Test.Tasty
import Test.Tasty.Golden
import Text.Trifecta.Parser
import Text.Trifecta.Result

import MLayout.Parser

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = foldM f ([], [])
    where
        f (ts, fs) x = do
            b <- p x
            return $ if b then (x:ts, fs) else (ts, x:fs)

traverseDir :: FilePath -> (FilePath -> IO Bool) -> IO [FilePath]
traverseDir root ok = go root
    where
        go :: FilePath -> IO [FilePath]
        go dir = do
            paths <- map (dir </>) <$> listDirectory dir
            (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
            oks <- filterM ok filePaths
            (oks ++) <$> (concat <$> mapM go dirPaths)

isMlayout :: FilePath -> IO Bool
isMlayout p = return $ "mlayout" `isExtensionOf` p

mkOutFile :: FilePath -> FilePath -> IO ()
mkOutFile i o = do
    doc <- parseFromFileEx parser i >>= \ case
        Success ok -> return $ vcat $ fmap pretty ok
        Failure e -> return $ reAnnotate (const ()) (_errDoc e <> line)
    withFile o WriteMode (`hPutDoc` doc)

mkTest :: FilePath -> TestTree
mkTest i = goldenVsFile name g o (mkOutFile i o)
    where
        o          = i <.> "out"
        g          = i <.> "golden"
        name       = takeBaseName i

main :: IO ()
main = do

    tests    <- testGroup "Tests"   <$> map mkTest <$> traverseDir "tests" isMlayout
    mlayouts <- testGroup "MLayout" <$> map mkTest <$> traverseDir "mlayout" isMlayout

    defaultMain $ testGroup "Everyting" [tests, mlayouts]
