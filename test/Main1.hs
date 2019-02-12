{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           MLayout.Parser
import           System.Environment
import           System.Exit
import           Text.Trifecta.Parser

main :: IO ()
main = getArgs >>= \ case
    (a1 : _) -> parseFromFile parser a1 >>= \ case
        Just x -> BSL.putStr $ encode x
        Nothing -> return ()
    _ -> do
        putStrLn "mind args"
        exitFailure
