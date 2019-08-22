{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Data.Text.IO as DTI
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Options.Applicative
import           System.Exit
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as TPP
import qualified Text.Trifecta.Parser as TRI
import qualified Text.Trifecta.Result as TRI

import           MLayout.Data
import           MLayout.Parser
import qualified MLayout.Resolver as MLR

data OutputType
    = Pretty
    | PrettyResolved
    | Format FilePath
    deriving Show

data MLayoutOptions
    = MLayoutOptions
        { outputTypeOpt :: OutputType
        , inFile  :: FilePath
        , outFile :: Maybe FilePath
        }

optsParser :: Parser MLayoutOptions
optsParser = MLayoutOptions
    <$> outputTypeParser
    <*> inFileParser
    <*> outFileParser
        where
            outputTypeParser = prettyOutputParser <|> prettyResolvedOutputParser <|> formatOutputParser
            prettyOutputParser = flag' Pretty
                (  long "pretty"
                <> short 'p'
                <> help "Pretty print"
                )
            prettyResolvedOutputParser = flag' PrettyResolved
                (  long "pretty-resolved"
                <> short 'P'
                <> help "Pretty print resolved tree"
                )
            formatOutputParser = Format <$> strOption
                (  long "format"
                <> short 'f'
                <> metavar "TEMPLATE"
                <> help "Format with template file"
                )
            inFileParser = argument str
                    (  metavar "INPUT_FILE"
                    <> help "Input file"
                    )
            outFileParser = optional (argument str
                    (  metavar "OUTPUT_FILE"
                    <> help "Output file or directory"
                    ))

opts :: ParserInfo MLayoutOptions
opts = info (helper <*> optsParser)
    (  fullDesc
    <> header "mlayout - transform memory layout files"
    <> progDesc
        (  "Transform input files in MLayout format into pretty printed MLayout files \
           \or to results of applying dhall function from TEMPLATE file to the syntax trees. \
           \The utility reads INPUT_FILE, processes it and outputs the result \
           \to OUTPUT_FILE or to stdout."
        )
    )

prettyPrint :: Pretty p => [p] -> Handle -> IO ()
prettyPrint layout h = hPutDoc h $ vcat $ fmap pretty layout

parse :: FilePath -> IO [MemoryItemParsed]
parse inf = TRI.parseFromFileEx parser inf >>= \ case
            TRI.Success ok -> return ok
            TRI.Failure xs  -> do
                TPP.displayIO stderr $ TPP.renderPretty 0.8 80 $ (TRI._errDoc xs) <> TPP.linebreak
                exitWith $ ExitFailure 1

resolve :: [MemoryItemParsed] -> IO [MemoryItemResolved]
resolve unresolvedLayout = either handleResolverError return (MLR.resolve unresolvedLayout)
    where
       handleResolverError (MLR.ResolverException t) = do
           DTI.hPutStrLn stderr t
           exitWith $ ExitFailure 1

mlayout :: MLayoutOptions ->  IO ()
mlayout MLayoutOptions {..} =
    let
        withOutputHandle :: (Handle -> IO ()) -> IO ()
        withOutputHandle f = case outFile of
            Just outputPath -> withFile outputPath WriteMode f
            Nothing -> f stdout

    in do
        parsed <- parse inFile
        case outputTypeOpt of
            Pretty -> do
                withOutputHandle $ prettyPrint parsed
            PrettyResolved -> do
                resolved <- resolve parsed
                withOutputHandle $ prettyPrint resolved
            Format _ -> undefined

main :: IO ()
main = execParser opts >>= mlayout
