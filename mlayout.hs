{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Control.Monad.IO.Class
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Options.Applicative
import           System.Exit
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as TPP
import qualified Text.Trifecta.Parser as TRI
import qualified Text.Trifecta.Result as TRI

import qualified MLayout.Parser as MLP
-- import qualified MLayout.Resolver as MLR

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
                    (  metavar "OUTPUT_FILE_OR_DIR"
                    <> help "Output file or directory"
                    ))

opts :: ParserInfo MLayoutOptions
opts = info (helper <*> optsParser)
    (  fullDesc
    <> header "mlayout - transform memory layout files"
    <> progDesc
        (  "Transform input files in MLayout format into pretty printed MLayout files, \
           \or TEMPLATE files processed by dhall engine. \
           \The utility reads INPUT_FILE, processes it and outputs the result \
           \to OUTPUT_FILE or to stdout."
        )
    )

prettyPrint :: Pretty p => [p] -> Handle -> IO ()
prettyPrint layout h = hPutDoc h $ vcat $ fmap pretty layout

mlayout :: MLayoutOptions -> ((Handle -> IO ()) -> IO ()) -> IO ()
mlayout MLayoutOptions {..} withOutputHandle = case outputTypeOpt of
    Pretty -> do
        parsed <- TRI.parseFromFileEx MLP.parser inFile >>= \ case
            TRI.Success ok -> return ok
            TRI.Failure xs  -> do
                liftIO $ TPP.displayIO stderr $ TPP.renderPretty 0.8 80 $ (TRI._errDoc xs) <> TPP.linebreak
                exitWith $ ExitFailure 1
        withOutputHandle $ prettyPrint parsed
    PrettyResolved -> undefined
    Format _ -> undefined

mlayout' :: MLayoutOptions -> IO ()
mlayout' options@(MLayoutOptions {..}) = case outFile of
    Just outf -> mlayout options (withFile outf WriteMode)
    Nothing -> mlayout options (\ f -> f stdout)

main :: IO ()
main = execParser opts >>= mlayout'
