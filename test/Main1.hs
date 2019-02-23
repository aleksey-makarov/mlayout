{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           MLayout.Parser
import           System.Environment
import           System.Exit
import           Text.Trifecta.Parser
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Util

main :: IO ()
main = getArgs >>= \ case
    (a1 : _) -> parseFromFile parser a1 >>= \ case
        Just x -> mapM_ (putDocW 80 . (<> line) . pretty) x
        Nothing -> return ()
    _ -> do
        putStrLn "mind args"
        exitFailure
