{-# LANGUAGE DataKinds, ExtendedDefaultRules, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards, TemplateHaskell      #-}
module Main where
import Language.Haskell.TH.Expand

import           Control.Monad                (forM_)
import           Control.Monad                (when)
import           Control.Monad.Trans          (liftIO)
import           Data.Monoid                  ((<>))
import           Data.Proxy                   (Proxy (..))
import           Data.String                  (fromString)
import qualified Data.Text                    as T
import           Language.Haskell.Exts        (Decl (..), ImportDecl (..))
import           Language.Haskell.Exts        (Module (..), ModuleName (..))
import           Language.Haskell.Exts        (ParseResult (ParseOk), parseFile)
import           Language.Haskell.Exts        (ParseMode (..), prettyPrint)
import           Language.Haskell.Exts        (KnownExtension (PackageImports))
import           Language.Haskell.Exts        (defaultParseMode)
import           Language.Haskell.Exts        (fromParseResult,
                                               parseModuleWithMode)
import           Language.Haskell.Exts        (Extension (EnableExtension))
import           Language.Haskell.Exts.QQ     (hs)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Options.Declarative          (Arg, Cmd, Def, Flag, get)
import qualified Options.Declarative          as Opt
import           Shelly                       (canonic, cp, fromText, ls, pwd)
import           Shelly                       (run_, shelly, silently, test_e)
import           Shelly                       (toTextIgnore, withTmpDir)
import           Shelly                       (cd, echo, echo_err, mv, readfile,
                                               writefile, (</>))
import           Shelly                       (rm)
import           System.FilePath              (takeBaseName, takeDirectory,
                                               takeFileName)
import           System.Random                (randomIO)

default (String)


main :: IO ()
main = Opt.run_ doMain

mkWrite out = [hs|Language.Haskell.TH.runIO . System.IO.writeFile $(liftHSE out)|]

finalize out m = [hs|
  do let write = $(mkWrite out)
         orig  = $(liftHSE m)
     mdic <- Language.Haskell.TH.Traced.tracing_ Language.Haskell.TH.Syntax.getQ
     case mdic of
       Nothing  -> write $ Language.Haskell.Exts.prettyPrint orig
       Just dic -> do
         write $ Language.Haskell.Exts.prettyPrint $
                 Language.Haskell.TH.Expand.expandTH dic orig

|]


doMain :: Flag "g" '["ghc"] "PATH" "pass to ghc" (Def "ghc" FilePath)
       -> Flag "a" '["with-args"] "STR" "argument(s) to pass to ghc" (Def "" String)
       -> Flag "o" '[] "STR" "argument(s) to pass to ghc" (Maybe String)
       -> Arg "TARGET" FilePath
       -> Cmd "Expanding Template Haskell using GHC command" ()
doMain ghcCmd opts moutc target = liftIO $ do
  let ghc  = get ghcCmd
      args = get opts
      moup = get moutc
  ParseOk m <- parseFile $ get target
  shelly $ silently $ do
    cwd <- pwd
    origPath <- takeDirectory . T.unpack . toTextIgnore <$> canonic (fromString $ get target)
    withTmpDir $ \dir -> do
      cd dir
      nonce <- liftIO randomIO
      let Module loc n@(ModuleName mn) ps mws mes is ds = traceTHSplices m
          is' = map (\n -> ImportDecl noLoc (ModuleName n) True False False Nothing Nothing Nothing)
                ["Language.Haskell.Exts", "GHC.Base", "GHC.Types", "Language.Haskell.TH"
                ,"Language.Haskell.Exts.Annotated.Syntax", "System.IO"]
          tbmname = "M" ++ show (abs nonce + 1 :: Int)
          tmpout = dir </> (tbmname ++ ".hs" :: String)
          addFin = [hs|do
            Language.Haskell.TH.Syntax.addModFinalizer $(finalize (T.unpack $ toTextIgnore tmpout) m)
            return []|]
          modified = Module loc n ps mws mes (is' ++ is)
                     (SpliceDecl noLoc addFin : ds)
          fname = takeFileName $ get target
          dest = dir </> fname
          searchPath = "-i" <> toTextIgnore dir <> ":" <> toTextIgnore cwd
      writefile dest $ T.pack $ prettyPrint modified
      run_ (fromText $ T.pack ghc) (map T.pack (words args)
                                    ++ [searchPath, "-dynamic", "-c", toTextIgnore dest])
      run_ (fromText $ T.pack ghc) (map T.pack (words args)
                                    ++ [searchPath, "-dynamic", "-ddump-minimal-imports", "-c", toTextIgnore tmpout])
      let imps = dir </> (mn ++ ".imports")
      imps_ok <- test_e imps
      when imps_ok $ do
        il <- readfile imps
        let ParseOk (Module _ _ _ _ _ idecls _) = parseModuleWithMode
                             defaultParseMode { extensions = [EnableExtension PackageImports] } $ T.unpack il
        ParseOk (Module i p c f d _ u) <- liftIO $ parseFile $ T.unpack $ toTextIgnore tmpout
        writefile tmpout $ T.pack $ prettyPrint $ Module i p c f d idecls u
      chs <- ls dir
      forM_ (filter (\a -> a `notElem` [dest, tmpout, imps] && takeBaseName (T.unpack $ toTextIgnore a) == tbmname) chs) $ \f -> cp f (fromString origPath)
      case moup of
        Nothing -> echo =<< readfile tmpout
        Just fp -> mv tmpout (fromText $ T.pack fp)
    return ()
