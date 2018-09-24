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
mapMaybe func ma = do
  v <- fmap func ma
  case v of
    Just b -> return b
    Nothing -> mzero

testCaseOk :: String -> String -> TestTree
testCaseOk name path = testCase name $ parseFromFileEx parser path >>= \ case
  Success _ -> return ()
  Failure e -> assertFailure $ "some error: " ++ (show $ _errDoc e)

testCaseError :: String -> String -> TestTree
testCaseError name path = testCase name $ parseFromFile parser path >>= \ case
  Just _  -> assertFailure "shold be error"
  Nothing -> return ()

makeTestCase :: FilePath -> Maybe TestTree
makeTestCase p =
  if (dropExtension p) `hasExtension` "err"
    then Just $ testCaseError testName ps
    else if p `hasExtension` "mlayout"
      then Just $ testCaseOk testName ps
      else Nothing
  where
    testName = encodeString $ basename p
    ps = encodeString p

main :: IO ()
main = do
  tests <- testGroup "Tests" <$> fold (mapMaybe makeTestCase $ ls "test") list
  defaultMain tests

  -- result <- parseFromFile parser "test/trifecta.txt"
  -- result <- parseFromFile parser "examples/example.mlayout"
  -- case result of
  --   Nothing -> exitFailure
  --   Just x  -> do { print x; exitSuccess }



