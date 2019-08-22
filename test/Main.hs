{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Control.Monad
import           Data.List as L
import           Pipes as P
import           Pipes.Prelude as P
import           Pipes.Safe as PS
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Directory
import           System.Posix.Files
import           System.Process.Typed
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

-- https://stackoverflow.com/questions/44267928/listing-all-the-files-under-a-directory-recursively-using-pipes

rmIfExists :: FilePath -> IO ()
rmIfExists f = do
    b <- fileExist f
    when b $ removeLink f

callMLayout :: String -> FilePath -> FilePath -> FilePath -> IO ()
callMLayout flag i o e = do
    rmIfExists o
    rmIfExists e
    ec <- withFile o WriteMode (\ oh ->
         withFile e WriteMode (\ eh -> do
            let cfg = setStdout (useHandleClose oh)
                    $ setStderr (useHandleClose eh)
                    $ proc "mlayout" [flag, i]
            runProcess cfg))
    case ec of
        ExitSuccess   -> rmIfExists e
        ExitFailure _ -> rmIfExists o

mkTestParseOk :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> TestTree
mkTestParseOk idir iname odir gdir oname = goldenVsFile (oname ++ " (parse)") g o mk
    where
        i = idir </> iname
        o = odir </> oname <.> "p" <.> "mlayout"
        e = odir </> oname <.> "p" <.> "err"
        g = gdir </> oname <.> "p" <.> "mlayout"
        mk = callMLayout "-p" i o e

mkTestResolveOk :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> TestTree
mkTestResolveOk idir iname odir gdir oname = goldenVsFile (oname ++ " (resolve)") g o mk
    where
        i = idir </> iname
        o = odir </> oname <.> "r" <.> "mlayout"
        e = odir </> oname <.> "r" <.> "err"
        g = gdir </> oname <.> "r" <.> "mlayout"
        mk = callMLayout "-P" i o e

mkTestParseErr :: FilePath -> FilePath -> FilePath -> FilePath -> TestTree
mkTestParseErr idir iname odir oname = testCase (oname ++ " (parse err)") $ do
    callMLayout "-p" i o e
    b <- fileExist e
    unless b $ assertFailure "should fail"
    where
        i = idir </> iname
        o = odir </> oname <.> "p" <.> "mlayout"
        e = odir </> oname <.> "p" <.> "err"

mkTestResolveErr :: FilePath -> FilePath -> FilePath -> FilePath -> TestTree
mkTestResolveErr idir iname odir oname = testCase (oname ++ " (resolve err)") $ do
    callMLayout "-P" i o e
    b <- fileExist e
    unless b $ assertFailure "should fail"
    where
        i = idir </> iname
        o = odir </> oname <.> "r" <.> "mlayout"
        e = odir </> oname <.> "r" <.> "err"

lsPipe :: FilePath -> Producer' FilePath (PS.SafeT IO) ()
lsPipe dirpath =
    PS.bracket (openDirStream dirpath) closeDirStream (forever . lsPipe')
        >-> P.takeWhile (/= "")
        >-> P.filter (not . flip L.elem [".", ".."])
    where
        lsPipe' stream = liftIO (readDirStream stream) >>= yield

treePipe :: FilePath -> Producer' FilePath (PS.SafeT IO) ()
treePipe path = lsPipe path >-> forever treePipe'
    where
        treePipe' = do
                entry <- await
                let entry' = path </> entry
                status <- liftIO $ getSymbolicLinkStatus entry'
                when (isDirectory status && (not $ isSymbolicLink status)) (treePipe entry' >-> P.map (entry </>))
                when (isRegularFile status) (yield entry)

mkTestFile :: MonadIO m => FilePath -> FilePath -> FilePath -> Pipe FilePath TestTree m ()
mkTestFile d o g = forever mkTestFile'
    where
        mkTestFile' = do
            f <- await
            let f' = dropExtension f
            if "E" `isExtensionOf` f'
                         then
                             let f'' = dropExtension f'
                             in yield $ mkTestParseErr   d f o   f''
                         else if "e" `isExtensionOf` f'
                            then do
                                let f'' = dropExtension f'
                                yield $ mkTestParseOk    d f o g f''
                                yield $ mkTestResolveErr d f o   f''
                            else do
                                yield $ mkTestParseOk    d f o g f'
                                yield $ mkTestResolveOk  d f o g f'

mkTestsDir :: FilePath -> FilePath -> FilePath -> IO [TestTree]
mkTestsDir d o g = PS.runSafeT $ P.toListM (treePipe d >-> (P.filter $ isExtensionOf "mlayout") >-> mkTestFile d o g)

main :: IO ()
main = do
    tests    <- testGroup "Tests"   <$> mkTestsDir "test/test" "test/out.test"    "test/gold.test"
    examples <- testGroup "MLayout" <$> mkTestsDir "mlayout"   "test/out.mlayout" "test/gold.mlayout"

    defaultMain $ testGroup "Everyting" [tests, examples]
