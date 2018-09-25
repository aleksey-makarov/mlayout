{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           MLayout.Parser
import           System.Environment
import           System.Exit
import           Text.Trifecta.Parser

main :: IO ()
main = getArgs >>= \ case
    (a1 : _) -> parseFromFile parser a1 >>= \ case
        Just x -> print x
        Nothing -> return ()
    _ -> do
        putStrLn "mind args"
        exitFailure
