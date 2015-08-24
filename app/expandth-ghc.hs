{-# LANGUAGE DataKinds, OverloadedStrings, QuasiQuotes, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell                                            #-}
module Main where
import Language.Haskell.TH.Expand

import           Control.Monad                (forM_)
import           Control.Monad.Trans          (liftIO)
import           Data.Monoid                  ((<>))
import           Data.String                  (fromString)
import qualified Data.Text                    as T
import           Language.Haskell.Exts        (Decl (..), ImportDecl (..))
import           Language.Haskell.Exts        (Module (..), ModuleName (..))
import           Language.Haskell.Exts        (ParseResult (ParseOk), parseFile)
import           Language.Haskell.Exts        (prettyPrint)
import           Language.Haskell.Exts.QQ     (hs)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Options.Declarative          (Arg, Cmd, Def, Flag, get)
import qualified Options.Declarative          as Opt
import           Shelly                       (canonic, cp, fromText, ls, pwd)
import           Shelly                       (run_, shelly, silently)
import           Shelly                       (toTextIgnore, withTmpDir)
import           Shelly                       (echo, mv, readfile, writefile,
                                               (</>))
import           System.FilePath              (takeDirectory, takeFileName)
import           System.Random                (randomIO)


main :: IO ()
main = Opt.run_ doMain

mkWrite out = [hs|Language.Haskell.TH.runIO . writeFile $(liftHSE out)|]

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
      nonce <- liftIO randomIO
      let Module loc n ps mws mes is ds = traceTHSplices m
          is' = map (\n -> ImportDecl noLoc (ModuleName n) True False False Nothing Nothing Nothing)
                ["Language.Haskell.Exts", "GHC.Base", "GHC.Types", "Language.Haskell.TH"
                ,"Language.Haskell.Exts.Annotated.Syntax"]
          tmpout = dir </> ("M" ++ (show (abs nonce + 1 :: Int)) ++ ".hs" :: String)
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
                                    ++ ["-i" , "" , searchPath, "-dynamic", "-c", toTextIgnore dest])
      chs <- ls dir
      forM_ (filter (`notElem` [dest, tmpout]) chs) $ \f -> cp f (fromString origPath)
      case moup of
        Nothing -> echo =<< readfile tmpout
        Just fp -> mv tmpout (fromText $ T.pack fp)
    return ()
