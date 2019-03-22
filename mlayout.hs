{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import qualified MLayout.Parser as ML
import           Options.Applicative
import           System.Exit
import           System.FilePath
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as TPP
import qualified Text.Trifecta.Parser as TRI
import qualified Text.Trifecta.Result as TRI

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
        outSuffixFromOutputType = case outputType of
            Pretty   -> "mlayout"
            JSON     -> "json"
            Format t -> takeBaseName t
        outSuffix = maybe outSuffixFromOutputType id outSuffix'
        fileNameInToOut name = outDir </> replaceExtension (takeBaseName name) outSuffix

        parseFile :: FilePath -> IO [ML.MLayout]
        parseFile inFile = TRI.parseFromFileEx ML.parser inFile >>= \ case
            TRI.Success ok -> return ok
            TRI.Failure xs  -> do
                liftIO $ TPP.displayIO stderr $ TPP.renderPretty 0.8 80 $ (TRI._errDoc xs) <> TPP.linebreak
                exitWith $ ExitFailure 1

        putDocFile :: FilePath -> Doc ann -> IO ()
        putDocFile path doc = withFile path WriteMode putDocFile'
            where
                putDocFile' :: Handle -> IO ()
                putDocFile' h = hPutDoc h doc

        prettyPrint :: Pretty p => FilePath -> [p] -> IO ()
        prettyPrint inFile layout = putDocFile (fileNameInToOut inFile) $ vcat $ fmap pretty layout

        printJSON :: ToJSON j => FilePath -> [j] -> IO ()
        printJSON _ _ = return ()

        prepareAction :: OutputType -> IO (FilePath -> [ML.MLayout] -> IO ())
        prepareAction Pretty = return prettyPrint
        prepareAction JSON = return printJSON
        prepareAction (Format inFile) = do
            putStrLn $ "prepare action for template " ++ inFile ++ " NOT IMPLEMENTED"
            return (\ _ _ -> return ())

    print outputType
    print inFiles
    print outDir
    print outSuffix
    let
        printAction name = putStrLn $ name ++ " -> " ++ fileNameInToOut name
    mapM_ printAction inFiles

    outputAction <- prepareAction outputType

    let
        doFile inFile = do
            parsed <- parseFile inFile
            outputAction (fileNameInToOut inFile) parsed

    mapM_ doFile inFiles

    exitWith ExitSuccess

main :: IO ()
main = execParser opts >>= mlayout
