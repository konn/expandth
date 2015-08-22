{-# LANGUAGE DefaultSignatures, DeriveDataTypeable, DeriveGeneric      #-}
{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, ViewPatterns              #-}
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-unused-binds #-}
module Main where
import Interpreter
import Orphans     (LiftHSE (..))
import THUtil      (SpliceInfo, srcPPPDic)
import Translate

import           Control.Arrow                       (second)
import           Control.Exception                   (throwIO)
import           Control.Monad.State
import           Data.Generics                       (Typeable, everywhere,
                                                      everywhereM, extM, extT,
                                                      mkM, mkT)
import           Data.List                           (intercalate)
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.String
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           GHC.Paths
import           Language.Haskell.Exts
import qualified Language.Haskell.Exts               as HSE
import           Language.Haskell.Exts.QQ            (decs, hs, ty)
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Interpreter        (OptionVal (..), eval,
                                                      infer,
                                                      installedModulesInScope,
                                                      interpret, runInterpreter,
                                                      searchPath, set)
import qualified Language.Haskell.Interpreter        as GHC
import           Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import           Language.Haskell.TH.Instances       ()
import           Shelly                              (cmd, shelly, silently,
                                                      test_d)
import           System.Environment                  (getArgs, setEnv)
import           System.IO                           (hPutStrLn, stderr)
import           System.IO.Temp

default (Text)

data SpliceKind = E | T | D

toSpliceE (IdSplice n) = var $ name n
toSpliceE (ParenSplice e) = e

wrapSplice kind sp    = ParenSplice $ wrapSplice' kind $ toSpliceE sp

spliceExp (SpliceExp sp) = SpliceExp $ wrapSplice E sp
spliceExp e = e

spliceType (TySplice sp) = TySplice $ wrapSplice T sp
spliceType e = e

wrapSplice' :: SpliceKind -> HSE.Exp -> HSE.Exp
wrapSplice' kind e =
  let upd = case kind of
        E -> [hs|\th s dic -> dic { PPPDic.expDic = Data.Map.insertWith (++) $(liftHSE e)
                                          [(Language.Haskell.TH.Traced.topDecls s, th)] (PPPDic.expDic dic) } |]
        T -> [hs|\th s dic -> dic { PPPDic.typeDic = Data.Map.insertWith (++) $(liftHSE e)
                                         [(Language.Haskell.TH.Traced.topDecls s, th)] (PPPDic.typeDic dic) } |]
        D -> [hs|\th s dic -> dic { PPPDic.decDic = Data.Map.insertWith (++) $(liftHSE e)
                                          [(Language.Haskell.TH.Traced.topDecls s, th)] (PPPDic.decDic dic) } |]
  in [hs|
  do (__thv, s) <- Language.Haskell.TH.Traced.tracing $(e)
     Language.Haskell.TH.Traced.tracing_ $ do
       mdic <- Language.Haskell.TH.Syntax.getQ :: Language.Haskell.TH.Syntax.Q (Maybe PPPDic.PPPDic)
       let dic = ($(upd) __thv s) $ maybe PPPDic.empty id mdic
       putQ dic
     sequence_ (Language.Haskell.TH.Traced.finalizers  s)
     return __thv
     |]


tyExp = [ty|Language.Haskell.Exts.Syntax.Exp|]

wrapSpliceDecl (SpliceDecl loc e) = SpliceDecl loc $ wrapSplice' D e
wrapSpliceDecl d = d

addTH (Module loc n ps mwarn mex is ds) =
  Module loc n (LanguagePragma noLoc [name "TemplateHaskell", name "DeriveDataTypeable"] : ps)
         mwarn mex (th ++ is) (ds ++ final)
  where
    spliced = [hs|
do mpdic <- Language.Haskell.TH.Traced.tracing_ Language.Haskell.TH.Syntax.getQ
            :: Language.Haskell.TH.Syntax.Q (Maybe PPPDic.PPPDic)
   let (PPPDic.PPPDic e t d) = maybe PPPDic.empty id mpdic
   Language.Haskell.TH.tupE [Language.Haskell.TH.Syntax.lift (Data.Map.toList e)
        ,Language.Haskell.TH.Syntax.lift (Data.Map.toList t)
        ,Language.Haskell.TH.Syntax.lift (Data.Map.toList d)
        ]
    |]
    final = [decs|
      ___table :: ([(Language.Haskell.Exts.Syntax.Exp, [([Language.Haskell.TH.Syntax.Dec], Language.Haskell.TH.Syntax.Exp)])]
                  ,[(Language.Haskell.Exts.Syntax.Exp, [([Language.Haskell.TH.Syntax.Dec], Language.Haskell.TH.Syntax.Type)])]
                  ,[(Language.Haskell.Exts.Syntax.Exp, [([Language.Haskell.TH.Syntax.Dec], [Language.Haskell.TH.Syntax.Dec])])])
      ___table = $(SpliceExp $ ParenSplice spliced)
    |]
    th :: [ImportDecl]
    th = [ ImportDecl noLoc (ModuleName "Language.Haskell.TH") False False False Nothing Nothing
                    (Just (False, [IAbs $ name "runIO" , IAbs $ name "pprint", IAbs $ name "reportWarning", IAbs $ name "location"]))
         , ImportDecl noLoc (ModuleName "Language.Haskell.TH.Syntax") False False False Nothing Nothing
                    (Just (False, [IAbs $ name "getQ", IAbs $ name "putQ"]))
         , ImportDecl noLoc (ModuleName "Language.Haskell.TH.Syntax") True False False Nothing Nothing
                    Nothing
         , ImportDecl noLoc (ModuleName "Language.Haskell.TH") True False False Nothing Nothing
                    Nothing
         , ImportDecl noLoc (ModuleName "Language.Haskell.TH.Quote") True False False Nothing Nothing
                    Nothing
         , ImportDecl noLoc (ModuleName "Language.Haskell.TH.Traced") True False False Nothing Nothing
                    Nothing
         , ImportDecl noLoc (ModuleName "Language.Haskell.TH.Instances") False False False Nothing Nothing
                    (Just (False, []))
         , ImportDecl noLoc (ModuleName "Language.Haskell.Exts.Syntax") True False False Nothing Nothing Nothing
         , ImportDecl noLoc (ModuleName "Language.Haskell.Exts") True False False Nothing Nothing Nothing
         , ImportDecl noLoc (ModuleName "Language.Haskell.Exts.SrcLoc") True False False Nothing Nothing Nothing
         , ImportDecl noLoc (ModuleName "Data.Typeable") True False False Nothing Nothing Nothing
         , ImportDecl noLoc (ModuleName "Data.Map") True False False Nothing Nothing Nothing
         , ImportDecl noLoc (ModuleName "PPPDic") True False False Nothing Nothing Nothing
         ]

main :: IO ()
main = do
  fp : _ <- getArgs
  ParseOk m <- parseFile fp
  let exts = extractExts m
      modified = addTH $ everywhere (mkT spliceExp
                                     `extT` spliceType
                                     `extT` wrapSpliceDecl
                                    ) m
  dbs <- shelly $ silently $ do
    snapPkgDb <- init . T.unpack <$> cmd "stack" "path" "--snapshot-pkg-db"
    loclPkgDb <- init . T.unpack <$> cmd "stack" "path" "--local-pkg-db"
    filterM (test_d . fromString) [snapPkgDb, loclPkgDb, libdir ++ "/package.conf.d"]
  setEnv "GHC_PACKAGE_PATH" $ intercalate ":" dbs
  ans <- runInterpreter $ withSystemTempDirectory "expandth" $ \dir -> do
    ps <- GHC.get searchPath
    set [searchPath := (dir : ps)]
    liftIO $ writeFile "PPPDic.hs" srcPPPDic
    mapM_ (unsafeSetGhcOption . ("-package-db " ++)) dbs
    set [installedModulesInScope  := True]
    liftIO $ writeFile (fp ++ ".alt") $ prettyPrint modified
    withHSEModule modified $ interpret "___table" infer
  case ans :: Either GHC.InterpreterError SpliceInfo of
    Left (GHC.WontCompile errs) -> mapM_ (putStrLn . GHC.errMsg) errs
    Left exc -> throwIO exc
    Right (exs, ts, decs) -> do
      let edic = M.fromListWith (++) $ map (second $ map snd) exs
          tdic = M.fromListWith (++) $ map (second $ map snd) ts
          ddic = M.fromListWith (++) $ map (second $ map snd) decs
          p' = evalState (everywhereM (mkM (transE exts) `extM` transD exts) $ m) (edic, tdic, ddic)
      liftIO $ putStrLn $ prettyPrint p'

transE exts e0@(SpliceExp (toSpliceE -> e)) = do
  m <- gets (\(a,_,_) -> M.lookup e a)
  case m of
    Nothing -> return e0
    Just [] -> modify (\(a,b,c) -> (M.delete e a, b,c)) >> return e0
    Just (x:xs) -> do
      modify (\(a,b,c) -> (M.update mtail e a, b, c))
      return $ fromMaybe e0 $ toHSEExp exts x
transE _ e = return e

transD exts (e0@(SpliceDecl _ e) : drest) = do
  m <- gets (\(_,_,a) -> M.lookup e a)
  case m of
    Nothing -> (:) e0 <$> transD exts drest
    Just [] -> modify (\(a,b,c) -> (a, b, M.delete e c)) >> ((:) e0  <$> transD exts drest)
    Just (x:xs) -> do
      modify (\(a,b,c) -> (a, b, M.update mtail e c))
      (++) (fromMaybe [e0] $ toHSEDecs exts x) <$> transD exts drest
transD exts (e : es) = (++) [e] <$> transD exts es
transD _ [] = return []


mtail [] =  Nothing
mtail xs = Just $ tail xs

extractExts (Module _ _ ps _ _ _ _) = do
  [parseExtension (prettyPrint n) | LanguagePragma  _ ns <- ps,  n <- ns]
