{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           MLayout.Parser
import           System.Environment
import           System.Exit
import           Text.Trifecta.Parser

main :: IO ()
main = getArgs >>= \ case
    (a1 : _) -> parseFromFile parser a1 >>= \ case
        Just x -> BSL8.putStrLn $ encodePretty x
        Nothing -> return ()
    _ -> do
        putStrLn "mind args"
        exitFailure
