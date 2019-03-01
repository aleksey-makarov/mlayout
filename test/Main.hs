{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Foldl hiding (fold, mapM_)
import           Data.Text.Prettyprint.Doc hiding (list)
import           Data.Text.Prettyprint.Doc.Render.Text
import           Prelude hiding (FilePath)
import           MLayout.Parser
import           System.IO hiding(FilePath)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import qualified Text.PrettyPrint.ANSI.Leijen as PAL
import           Text.Trifecta.Parser
import           Text.Trifecta.Result
import           Turtle hiding (f, x, e)

putDocFile :: String -> Doc ann -> IO ()
putDocFile pathString doc = withFile pathString WriteMode putDocFile'
    where
        putDocFile' :: Handle -> IO ()
        putDocFile' h = hPutDoc h doc

putErrInfoFile :: String -> ErrInfo -> IO ()
putErrInfoFile pathString e = withFile pathString WriteMode putErrInfoFile'
    where
        doc = _errDoc e
        -- FIXME: don't print formatting symbols into the error file
        putErrInfoFile' h = PAL.hPutDoc h doc

rmIfExists :: FilePath -> Shell ()
rmIfExists p = do
    b <- testfile p
    when b $ rm p

makeTestCase :: FilePath -> Shell TestTree
makeTestCase path =
    if path `hasExtension` "mlayout"
        then do
          rmIfExists prettyPath
          rmIfExists errPath
          return $ if (dropExtension path) `hasExtension` "err"
            then mkErr
            else mkGoldPretty
        else mzero
    where
        pathString = encodeString path

        testPrettyName = (encodeString $ basename path) ++ " (pretty)"
        testErrorName = (encodeString $ basename path) ++ " (error)"

        prettyPath = path <.> "pretty"
        prettyPathString = encodeString prettyPath
        prettyGoldPathString = encodeString $ path <.> "pretty" <.> "gold"
        errPath = path <.> "err"
        errPathString = encodeString $ errPath

        mkOutOk :: Pretty d => [d] -> IO ()
        mkOutOk doc = putDocFile prettyPathString $ vcat $ fmap pretty doc

        mkOutPretty :: IO ()
        mkOutPretty = parseFromFileEx parser pathString >>= foldResult (putErrInfoFile errPathString) mkOutOk

        mkGoldPretty :: TestTree
        mkGoldPretty = goldenVsFile testPrettyName prettyGoldPathString prettyPathString mkOutPretty

        mkErr :: TestTree
        mkErr = testCase testErrorName $ do
            mkOutPretty
            b <- testfile errPath
            unless b $ assertFailure "should fail"

main :: IO ()
main = do
    tests    <- testGroup "Tests"    <$> fold (ls "test"     >>= makeTestCase) list
    examples <- testGroup "Examples" <$> fold (ls "examples" >>= makeTestCase) list

    defaultMain $ testGroup "Everyting" [tests, examples]
