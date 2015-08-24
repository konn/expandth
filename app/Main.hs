{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where
import Interpreter

import Language.Haskell.TH.Expand

import           Control.Exception                   (throwIO)
import           Control.Monad                       (filterM)
import           Control.Monad.Trans                 (liftIO)
import           Data.List                           (intercalate)
import           Data.String                         (fromString)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           GHC.Paths                           (libdir)
import           Language.Haskell.Exts               (ParseResult (..))
import           Language.Haskell.Exts               (parseFile, prettyPrint)
import           Language.Haskell.Interpreter        (OptionVal (..), infer)
import           Language.Haskell.Interpreter        (installedModulesInScope)
import           Language.Haskell.Interpreter        (interpret, runInterpreter)
import           Language.Haskell.Interpreter        (searchPath, set)
import qualified Language.Haskell.Interpreter        as GHC
import           Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import           Shelly                              (cmd, shelly, silently)
import           Shelly                              (test_d)
import           System.Environment                  (getArgs, setEnv)
import           System.IO.Temp

default (Text)

main :: IO ()
main = do
  fp : _ <- getArgs
  ParseOk m <- parseFile fp
  let modified = traceTHSplices m
  dbs <- shelly $ silently $ do
    snapPkgDb <- init . T.unpack <$> cmd "stack" "path" "--snapshot-pkg-db"
    loclPkgDb <- init . T.unpack <$> cmd "stack" "path" "--local-pkg-db"
    filterM (test_d . fromString) [snapPkgDb, loclPkgDb, libdir ++ "/package.conf.d"]
  setEnv "GHC_PACKAGE_PATH" $ intercalate ":" dbs
  ans <- runInterpreter $ withSystemTempDirectory "expandth" $ \dir -> do
    ps <- GHC.get searchPath
    set [searchPath := (dir : ps)]
    mapM_ (unsafeSetGhcOption . ("-package-db " ++)) dbs
    set [installedModulesInScope  := True]
    liftIO $ writeFile (fp ++ ".alt") $ prettyPrint modified
    withHSEModule modified $ interpret "___table" infer
  case ans of
    Left (GHC.WontCompile errs) -> mapM_ (putStrLn . GHC.errMsg) errs
    Left exc -> throwIO exc
    Right dic -> putStrLn $ prettyPrint $ expandTH dic m
