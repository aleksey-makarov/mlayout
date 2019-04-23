{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Foldl hiding (fold, mapM_)
import           Prelude hiding (FilePath)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Turtle hiding (f, x, e)

rmIfExists :: MonadIO io => FilePath -> io ()
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

        mkPretty :: IO ()
        mkPretty = do
            ec <- shell (format ("mlayout -p " % fp % " " % fp % " 2> " % fp) path prettyPath prettyErrPath) empty
            case ec of
                ExitSuccess -> rmIfExists prettyErrPath
                ExitFailure _ -> rmIfExists prettyPath

        mkJSON :: IO ()
        mkJSON = do
            ec <- shell (format ("mlayout -j " % fp % " " % fp % " 2> " % fp) path jsonPath jsonErrPath) empty
            case ec of
                ExitSuccess -> rmIfExists jsonErrPath
                ExitFailure _ -> rmIfExists jsonPath

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
