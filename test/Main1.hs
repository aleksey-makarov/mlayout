{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           MLayout.Parser
import           Text.Trifecta.Parser

main :: IO ()
-- FIXME: get test file name from command line
main = parseFromFile parser "test/test.ok.mlayout" >>= \ case
  Just x -> print x
  Nothing -> return ()
