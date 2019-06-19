{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Foldl hiding (fold, mapM_)
-- import           Data.Text (unpack)
import           Prelude hiding (FilePath)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Turtle hiding (f, x, e)

rmIfExists :: MonadIO io => FilePath -> io ()
rmIfExists p = do
    b <- testfile p
    when b $ rm p

makeTestCase :: FilePath -> FilePath -> FilePath -> Shell [TestTree]
makeTestCase outDir goldDir path =
    if path `hasExtension` "mlayout"
        then do
            rmIfExists errPath
            rmIfExists prettyPath
            -- mapM_ (rmIfExists . cPath) templates
            return $ if (dropExtension path) `hasExtension` "err"
                then [mkErr]
                else [mkGoldPretty] -- : mkGoldCs
        else mzero
    where

        pathBaseName = encodeString $ basename path
        pathFile = filename path

        testErrorName  = pathBaseName ++ " (error)"
        testPrettyName = pathBaseName ++ " (pretty)"
        -- testCName t    = unpack $ format (fp % " (" % fp % ")") (basename path) (filename t)

        -- templates :: [FilePath]
        -- templates = ["templates/c.ede"]

        errPath        = outDir  </> pathFile <.> "err"
        prettyGoldPath = goldDir </> pathFile <.> "pretty" <.> "gold"
        prettyPath     = outDir  </> pathFile <.> "pretty"
        -- cPath     t    = outDir  </> pathFile <.> (format fp $ basename t)
        -- cGoldPath t    = goldDir </> pathFile <.> (format fp $ basename t) <.> "gold"

        mkSomething :: Text -> FilePath -> IO ()
        mkSomething flag okPath = do
            ec <- shell (format ("mlayout " % s % " " % fp % " " % fp % " 2> " % fp) flag path okPath errPath) empty
            case ec of
                ExitSuccess -> rmIfExists errPath
                ExitFailure _ -> rmIfExists okPath

        mkPretty :: IO ()
        mkPretty = mkSomething "-p" prettyPath

        -- mkC :: FilePath -> IO ()
        -- mkC template = mkSomething (format ("-i FAKE_ID -f " % fp) template) (cPath template)

        mkGoldPretty :: TestTree
        mkGoldPretty = goldenVsFile testPrettyName (encodeString prettyGoldPath) (encodeString prettyPath) mkPretty

        -- mkGoldC :: FilePath -> TestTree
        -- mkGoldC template = goldenVsFile (testCName template)
        --                                (encodeString $ cGoldPath template)
        --                                (encodeString $ cPath template)
        --                                (mkC template)

        -- mkGoldCs :: [TestTree]
        -- mkGoldCs = fmap mkGoldC templates

        mkErr :: TestTree
        mkErr = testCase testErrorName $ do
            mkPretty
            b <- testfile errPath
            unless b $ assertFailure "should fail"

main :: IO ()
main = do
    putStrLn ""
    tests    <- (testGroup "Tests"    . concat) <$> fold (ls "test"     >>= makeTestCase "test/out"         "test/gold"        ) list
    examples <- (testGroup "MLayout"  . concat) <$> fold (ls "mlayout"  >>= makeTestCase "test/out/mlayout" "test/gold/mlayout") list

    defaultMain $ testGroup "Everyting" [tests, examples]
