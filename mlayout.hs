{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Util
import qualified MLayout.Parser as ML
import           Options.Applicative
import           System.FilePath
import           Text.Trifecta.Parser (parseFromFile)

data OutputType = Pretty | JSON | Format FilePath deriving Show

data MLayoutOptions = MLayoutOptions
    { outputType :: OutputType
    , outDir     :: FilePath
    , outSuffix' :: Maybe String
    , inFiles    :: [FilePath]
    }

optsParser :: Parser MLayoutOptions
optsParser = MLayoutOptions
    <$> ( prettyOutput <|> jsonOutput <|> formatOutput )
    <*> strOption
        (  long "output-dir"
        <> short 'o'
        <> metavar "OUT"
        <> help "Output directory"
        <> value "."
        )
    <*> optional (strOption
        (  long "suffix"
        <> short 's'
        <> metavar "SUFFIX"
        <> help "Output file suffix"
        ))
    <*> some (argument str
        (  metavar "FILES..."
        <> help "Input files"
        ))
        where
            prettyOutput = flag' Pretty
                (  long "pretty"
                <> short 'p'
                <> help "Pretty print"
                )
            jsonOutput = flag' JSON
                (  long "json"
                <> short 'j'
                <> help "Format as json"
                )
            formatOutput = Format <$> strOption
                (  long "format"
                <> short 'f'
                <> metavar "TEMPLATE"
                <> help "Format with template file"
                )

opts :: ParserInfo MLayoutOptions
opts = info (helper <*> optsParser)
    (  fullDesc
    <> header "mlayout - transform memory layout files"
    <> progDesc
        (  "Transform input files in MLayout format into pretty printed MLayout files, "
        ++ "JSON files, or TEMPLATE files processed by EDE engine. "
        ++ "Put the results into directoty OUTDIR replacing \'.mlayout\' suffixes with SUFFIX."
        )
    )

mlayout :: MLayoutOptions -> IO ()
mlayout MLayoutOptions {..} = do

    let
        (inFile : _) = inFiles
        outSuffixFromOutputType = case outputType of
            Pretty   -> "mlayout"
            JSON     -> "json"
            Format t -> takeBaseName t
        outSuffix = maybe outSuffixFromOutputType id outSuffix'

    print outputType
    print inFiles
    print outDir
    print outSuffix

    parseFromFile ML.parser inFile >>= \ case
        Just x -> mapM_ (putDocW 80 . (<> line) . pretty) x
        Nothing -> return ()

    -- result <- parseFromFile parser inFile
    -- case result of
    --     Nothing -> putStrLn "Failure"
    --     Just x  -> do
    --         putStrLn "Success"
    --         print x

main :: IO ()
main = execParser opts >>= mlayout
