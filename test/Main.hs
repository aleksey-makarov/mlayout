{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Foldl hiding (fold, mapM_)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
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

makeTestCase :: FilePath -> Shell [TestTree]
makeTestCase path =
    if path `hasExtension` "mlayout"
        then do
            rmIfExists prettyPath
            rmIfExists prettyErrPath
            rmIfExists jsonPath
            rmIfExists jsonErrPath
            return $ if (dropExtension path) `hasExtension` "err"
                then [mkErr]
                else [mkGoldPretty, mkGoldJSON]
        else mzero
    where

        testErrorName  = (encodeString $ basename path) ++ " (error)"
        testPrettyName = (encodeString $ basename path) ++ " (pretty)"
        testJSONName   = (encodeString $ basename path) ++ " (json)"

        prettyPath           = path <.> "pretty"
        prettyErrPath        = prettyPath <.> "err"
        prettyGoldPath       = prettyPath <.> "gold"

        jsonPath             = path <.> "json"
        jsonErrPath          = jsonPath <.> "err"
        jsonGoldPath         = jsonPath <.> "gold"

        mkPrettyOk :: Pretty d => [d] -> IO ()
        mkPrettyOk doc = putDocFile (encodeString prettyPath) $ vcat $ fmap pretty doc

        mkJSONOk :: ToJSON d => [d] -> IO ()
        mkJSONOk doc = BL.writeFile (encodeString jsonPath) $ encodePretty $ toJSONList doc

        mkPretty :: IO ()
        mkPretty = parseFromFileEx parser (encodeString path) >>= foldResult (putErrInfoFile (encodeString prettyErrPath)) mkPrettyOk

        mkJSON :: IO ()
        mkJSON = parseFromFileEx parser (encodeString path) >>= foldResult (putErrInfoFile (encodeString jsonErrPath)) mkJSONOk

        mkGoldPretty :: TestTree
        mkGoldPretty = goldenVsFile testPrettyName (encodeString prettyGoldPath) (encodeString prettyPath) mkPretty

        mkGoldJSON :: TestTree
        mkGoldJSON = goldenVsFile testJSONName (encodeString jsonGoldPath) (encodeString jsonPath) mkJSON

        mkErr :: TestTree
        mkErr = testCase testErrorName $ do
            mkPretty
            b <- testfile prettyErrPath
            unless b $ assertFailure "should fail"

main :: IO ()
main = do
    tests    <- (testGroup "Tests"    . concat) <$> fold (ls "test"     >>= makeTestCase) list
    examples <- (testGroup "MLayout"  . concat) <$> fold (ls "mlayout"  >>= makeTestCase) list

    defaultMain $ testGroup "Everyting" [tests, examples]
