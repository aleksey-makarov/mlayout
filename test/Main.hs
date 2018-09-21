{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           MLayout.Parser
import           Text.Trifecta.Parser
import           System.Exit

main :: IO ()
main = do
  -- result <- parseFromFile parser "test/trifecta.txt"
  result <- parseFromFile parser "examples/example.mlayout"
  case result of
    Nothing -> exitFailure
    Just x  -> do { print x; exitSuccess }



