{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Control.Monad.IO.Class
import           Data.Text
-- import           Data.Text.Lazy.Encoding
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
-- import           Data.Time.LocalTime
-- import           Data.Time.RFC822
import           Options.Applicative
import           System.Exit
import           System.FilePath
import           System.Posix.Files
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as TPP
import qualified Text.Trifecta.Parser as TRI
import qualified Text.Trifecta.Result as TRI

import qualified MLayout.Parser as MLP
import qualified MLayout.Resolver as MLR

data OutputType
    = Pretty
    | PrettyResolved
    | Format FilePath
    deriving Show

data InOutSpecification
    = SingleInputFile
        { inFileOpt       :: FilePath
        , outFileOrDirOpt :: Maybe FilePath
        }
--    | ManyInputFiles
--        { outDirOpt    :: FilePath
--        , outSuffixOpt :: Maybe String
--        , inFilesOpt   :: [FilePath]
--        }
    deriving Show

data MLayoutOptions
    = MLayoutOptions
        { outputTypeOpt :: OutputType
        , inOutOpt  :: InOutSpecification
        }

-- FIXME: add an option that sets base directory so that all the .layout files
-- under it will be formatted
-- FIXME: "-d -" should specify output to stdout
optsParser :: Parser MLayoutOptions
optsParser = MLayoutOptions
    <$> outputTypeParser
    <*> inOutSpecParser
        where
            outputTypeParser = prettyOutputParser <|> prettyResolvedOutputParser <|> formatOutputParser
            inOutSpecParser = singleInputFileParser -- <|> manyInputFilesParser
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
            singleInputFileParser = SingleInputFile
                <$> argument str
                    (  metavar "INPUT_FILE"
                    <> help "Input file"
                    )
                <*> optional (argument str
                    (  metavar "OUTPUT_FILE_OR_DIR"
                    <> help "Output file or directory"
                    ))
--            manyInputFilesParser = ManyInputFiles
--                <$> strOption
--                    (  long "output-dir"
--                    <> short 'd'
--                    <> metavar "OUTPUT_DIR"
--                    <> help "Output directory"
--                    )
--                <*> optional (strOption
--                    (  long "suffix"
--                    <> short 's'
--                    <> metavar "SUFFIX"
--                    <> help "Output file suffix"
--                    ))
--                <*> some (argument str
--                    (  metavar "FILES..."
--                    <> help "Input files"
--                    ))

opts :: ParserInfo MLayoutOptions
opts = info (helper <*> optsParser)
    (  fullDesc
    <> header "mlayout - transform memory layout files"
    <> progDesc
        (  "Transform input files in MLayout format into pretty printed MLayout files, "
        ++ "or TEMPLATE files processed by dhall engine. "
        ++ "The utility reads INPUT_FILE, processes it and outputs the result to OUTPUT_FILE_OR_DIR. "
--        ++ "If OUTPUT_DIR is specified, all FILES will be processed at one pass and written to files in that directory. "
--        ++ "The resulting files will have the same basename with suffix \'.mlayout\' changed to SUFFIX."
        ++ "ID_STRING replaces generated id string"
        )
    )

type WithFile = (Handle -> IO ()) -> IO ()

data Task
    = Task
        { inFile :: FilePath
        , withOutFile :: WithFile
        }

withOutFileStdout :: WithFile
withOutFileStdout f = f stdout

withOutFilePath :: FilePath -> WithFile
withOutFilePath path f = withFile path WriteMode f

-- FIXME: add \n to the end of output both to prettyPrint and printJSON
prettyPrint :: Pretty p => WithFile -> [p] -> IO ()
prettyPrint withFile' layout = withFile' f
    where
        f :: Handle -> IO ()
        f h = hPutDoc h $ vcat $ fmap pretty layout

fileExistsAndIsDir :: FilePath -> IO Bool
fileExistsAndIsDir f = do
    e <- fileExist f
    if not e
        then return False
        else isDirectory <$> getFileStatus f

mlayout :: MLayoutOptions -> IO ()
mlayout MLayoutOptions {..} = do

    -- print outputTypeOpt
    -- print inOutOpt

    -- let
    --    mkIdString :: IO Text
    --    mkIdString = formatTimeRFC822 <$> getZonedTime

    -- idString <- maybe mkIdString return idStringOpt

    let
        prepareOuputAction :: IO (FilePath -> WithFile -> [MLP.MLayout] -> IO ())
        prepareOuputAction = case outputTypeOpt of
            Pretty                   -> return (\ _ -> prettyPrint)
            PrettyResolved           -> undefined -- @m
            Format _templateFileName -> undefined

        parseFile :: FilePath -> IO [MLP.MLayout]
        parseFile inFile = TRI.parseFromFileEx MLP.parser inFile >>= \ case
            TRI.Success ok -> return ok
            TRI.Failure xs  -> do
                liftIO $ TPP.displayIO stderr $ TPP.renderPretty 0.8 80 $ (TRI._errDoc xs) <> TPP.linebreak
                exitWith $ ExitFailure 1

        outSuffix :: String
        outSuffix = case outputTypeOpt of
            Pretty          -> "mlayout"
            PrettyResolved  -> "mlayout"
            Format template -> takeBaseName template

        prepareBatch :: IO [Task]
        prepareBatch = case inOutOpt of
            -- return makes a singleton list, it's the same as (:[]) here
            -- ManyInputFiles {..} -> undefined
            SingleInputFile {..} -> (return . Task inFileOpt) <$> outFile
                where
                    outFile :: IO WithFile
                    outFile = case outFileOrDirOpt of
                        Just outFileOrDir -> do
                            dir <- fileExistsAndIsDir outFileOrDir
                            return $ withOutFilePath $ if dir
                                then outFileOrDir </> (takeBaseName inFileOpt) <.> outSuffix
                                else outFileOrDir
                        Nothing -> return withOutFileStdout

    outputAction <- prepareOuputAction

    let
        doTask Task {..} = do
            parsed <- parseFile inFile
            outputAction inFile withOutFile parsed

    prepareBatch >>= mapM_ doTask

    exitWith ExitSuccess

main :: IO ()
main = execParser opts >>= mlayout
