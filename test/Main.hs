{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Foldl hiding (fold)
import           Prelude hiding (FilePath)
import           MLayout.Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Trifecta.Parser
import           Text.Trifecta.Result
import           Turtle

makeTestCase :: FilePath -> Shell TestTree
makeTestCase p =
    if p `hasExtension` "mlayout"
        then return $ if (dropExtension p) `hasExtension` "err"
            -- FIXME: get one line description of error and pass it to assertFailure
            then mkTestCase (testName ++ " (E)") ps (assertFailure "should fail") (return ())
            else mkTestCase  testName            ps (return ())                   (assertFailure "should pass")
        else mzero
    where
        testName = encodeString $ basename p
        ps = encodeString p
        mkTestCase name path ok failure = testCase name $ parseFromFileEx parser path >>= foldResult (const failure) (const ok)

main :: IO ()
main = do
    tests    <- testGroup "Tests"    <$> fold (ls "test"     >>= makeTestCase) list
    examples <- testGroup "Examples" <$> fold (ls "examples" >>= makeTestCase) list

    defaultMain $ testGroup "Everyting" [tests, examples]
