{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Foldl hiding (fold, mapM_)
import           Prelude hiding (FilePath)
import           MLayout.Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Trifecta.Parser
import           Text.Trifecta.Result
import           Turtle

mapMaybe :: MonadPlus m => (a -> Maybe b) -> m a -> m b
mapMaybe func ma = fmap func ma >>= \ case
    Just b -> return b
    Nothing -> mzero

makeTestCase :: FilePath -> Maybe TestTree
makeTestCase p =
  if p `hasExtension` "mlayout"
    then Just $ if (dropExtension p) `hasExtension` "err"
      -- FIXME: get one line description of error and pass it to assertFailure
      then mkTestCase (testName ++ " (E)") ps (assertFailure "should fail") (return ())
      else mkTestCase  testName            ps (return ())                   (assertFailure "should pass")
    else Nothing
  where
    testName = encodeString $ basename p
    ps = encodeString p
    mkTestCase name path ok failure = testCase name $ parseFromFileEx parser path >>= foldResult (const failure) (const ok)

main :: IO ()
main = do
  tests <- testGroup "Tests" <$> fold (mapMaybe makeTestCase $ ls "test") list
  defaultMain tests
