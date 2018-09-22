{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           MLayout.Parser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Trifecta.Parser
import           Turtle

main :: IO ()
main = do
  view $ ls "test"
  let tests = testGroup "Everything" []
  defaultMain tests

  -- result <- parseFromFile parser "test/trifecta.txt"
  -- result <- parseFromFile parser "examples/example.mlayout"
  -- case result of
  --   Nothing -> exitFailure
  --   Just x  -> do { print x; exitSuccess }



