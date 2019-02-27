{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Foldl hiding (fold, mapM_)
import           Data.Text.Prettyprint.Doc hiding (list)
import           Data.Text.Prettyprint.Doc.Render.Text
import           Prelude hiding (FilePath)
import           MLayout.Parser
import           System.IO hiding(FilePath)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import qualified Text.PrettyPrint.ANSI.Leijen as PAL
import           Text.Trifecta.Parser
import           Text.Trifecta.Result
import           Turtle hiding (f, x, e)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

putDocFile :: String -> Doc ann -> IO ()
putDocFile pathString doc = withFile pathString WriteMode putDocFile'
  where
    putDocFile' :: Handle -> IO ()
    putDocFile' h = hPutDoc h doc

putErrInfoFile :: String -> ErrInfo -> IO ()
putErrInfoFile pathString e = withFile pathString WriteMode putErrInfoFile'
  where
    doc = _errDoc e
    -- FIXME: don't print formatting symbols into the error file
    putErrInfoFile' h = PAL.hPutDoc h doc

mkOut :: String -> IO ()
mkOut pathString = parseFromFileEx parser pathString >>= foldResult (putErrInfoFile errPathString) mkOutOk
    where
      mkOutOk x = putDocFile outPathString $ vcat $ fmap pretty x
      outPathString = pathString ++ ".out"
      errPathString = pathString ++ ".err"

mkGold :: FilePath -> Shell TestTree
mkGold path = do
  rmIfExists $ path <.> "out"
  rmIfExists $ path <.> "err"
  return $ goldenVsFile testName goldenString outString (mkOut pathString)
  where
    testName = (encodeString $ basename path) ++ " (G)"
    pathString = encodeString path
    outString = encodeString $ path <.> "out"
    goldenString = encodeString $ path <.> "gold"
    rmIfExists p = ifM (testfile p) (rm p) (return ())

makeTestCase :: FilePath -> Shell TestTree
makeTestCase path =
    if path `hasExtension` "mlayout"
        then ifM hasGold (mkGold path) $ if (dropExtension path) `hasExtension` "err"
            then return mkTestCaseErr
            else return mkTestCaseOk
        else mzero
    where
        testName = encodeString $ basename path
        pathString = encodeString path
        mkTestCase name ok failure = testCase name $ parseFromFileEx parser pathString >>= foldResult (const failure) (const ok)
        mkTestCaseOk = mkTestCase testName (return ()) (assertFailure "should pass")
        mkTestCaseErr = mkTestCase (testName ++ " (E)") (assertFailure "should fail") (return ())
        hasGold = testfile (path <.> "gold")

main :: IO ()
main = do
    tests    <- testGroup "Tests"    <$> fold (ls "test"     >>= makeTestCase) list
    examples <- testGroup "Examples" <$> fold (ls "examples" >>= makeTestCase) list

    defaultMain $ testGroup "Everyting" [tests, examples]
