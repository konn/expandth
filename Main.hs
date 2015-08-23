{-# LANGUAGE DefaultSignatures, DeriveDataTypeable, DeriveGeneric           #-}
{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, RankNTypes, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, ViewPatterns                   #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-type-defaults #-}
module Main where
import Interpreter
import Orphans     (LiftHSE (..))
import THUtil      (SpliceInfo, srcPPPDic)
import Translate

import           Control.Arrow                       (second)
import           Control.Exception                   (throwIO)
import           Control.Monad.State
import           Data.Generics                       (Data, everything,
                                                      everywhere, everywhereM,
                                                      extM, extT, mkM, mkQ, mkT)
import           Data.List                           (intercalate)
import           Data.List                           (nub, (\\))
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.String
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           GHC.Paths
import           Language.Haskell.Exts
import qualified Language.Haskell.Exts               as HSE
import           Language.Haskell.Exts.QQ            (decs, hs)
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Interpreter        (OptionVal (..), infer,
                                                      installedModulesInScope,
                                                      interpret, runInterpreter,
                                                      searchPath, set)
import qualified Language.Haskell.Interpreter        as GHC
import           Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import qualified Language.Haskell.TH                 as TH
import           Language.Haskell.TH.Instances       ()
import qualified Language.Haskell.TH.Syntax          as TH
import           Shelly                              (cmd, shelly, silently,
                                                      test_d)
import           System.Environment                  (getArgs, setEnv)
import           System.FilePath
import           System.IO.Temp

default (Text)

data SpliceKind = E | T | D
type RewriteState = (M.Map Exp [TH.Exp], M.Map Exp [TH.Type], M.Map Exp [[TH.Dec]])
type Rewriter a = forall m. Monad m => [Extension] -> a -> StateT RewriteState m a

toSpliceE :: Splice -> Exp
toSpliceE (IdSplice n) = var $ name n
toSpliceE (ParenSplice e) = e

wrapSplice :: SpliceKind -> Splice -> Splice
wrapSplice kind sp    = ParenSplice $ wrapSplice' kind $ toSpliceE sp

spliceExp :: Exp -> Exp
spliceExp (SpliceExp sp) = SpliceExp $ wrapSplice E sp
spliceExp e = e

spliceType :: Type -> Type
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


wrapSpliceDecl :: Decl -> Decl
wrapSpliceDecl (SpliceDecl l e) = SpliceDecl l $ wrapSplice' D e
wrapSpliceDecl d = d

addTH :: Module -> Module
addTH (Module l n ps mwarn mex is ds) =
  Module l n (LanguagePragma noLoc [name "TemplateHaskell", name "DeriveDataTypeable"] : ps)
         mwarn mex (qs ++ is) (ds ++ final)
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
    qs :: [ImportDecl]
    qs = [ ImportDecl noLoc (ModuleName "Language.Haskell.TH") False False False Nothing Nothing
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
    liftIO $ writeFile (dir </> "PPPDic.hs") srcPPPDic
    mapM_ (unsafeSetGhcOption . ("-package-db " ++)) dbs
    set [installedModulesInScope  := True]
    liftIO $ writeFile (fp ++ ".alt") $ prettyPrint modified
    withHSEModule modified $ interpret "___table" infer
  case ans :: Either GHC.InterpreterError SpliceInfo of
    Left (GHC.WontCompile errs) -> mapM_ (putStrLn . GHC.errMsg) errs
    Left exc -> throwIO exc
    Right (exs, ts, dcs) -> do
      let edic = M.fromListWith (++) $ map (second $ map snd) exs
          tdic = M.fromListWith (++) $ map (second $ map snd) ts
          ddic = M.fromListWith (++) $ map (second $ map snd) dcs
          topDecls = fromMaybe [] $ toHSEDecs exts $
                     concatMap (concatMap fst . snd) exs
                  ++ concatMap (concatMap fst . snd) ts
                  ++ concatMap (concatMap fst . snd) dcs
          p' = evalState (everywhereM (mkM (transE exts) `extM` transD exts `extM` transT exts) $ m) (edic, tdic, ddic)
          quals = nub $ concatMap (getModules . snd) exs
                     ++ concatMap (getModules . snd) ts
                     ++ concatMap (getModules . snd) dcs
          existing = getQualified m
          missing = [ ImportDecl noLoc (ModuleName n) True False False Nothing Nothing Nothing
                    | n <- quals \\ existing]
      liftIO $ putStrLn $ prettyPrint $ addImports missing $ addDecls topDecls p'

addDecls :: [Decl] -> Module -> Module
addDecls ds' (Module l n ps mw mex is ds) = Module l n ps mw mex is (ds ++ ds')

addImports :: [ImportDecl] -> Module -> Module
addImports is' (Module l n ps mw mex is ds) = Module l n ps mw mex (is ++ is') ds

transE :: Rewriter Exp
transE exts e0@(SpliceExp (toSpliceE -> e)) = do
  m <- gets (\(a,_,_) -> M.lookup e a)
  case m of
    Nothing -> return e0
    Just [] -> modify (\(a,b,c) -> (M.delete e a, b,c)) >> return e0
    Just (x:xs) -> do
      modify (\(a,b,c) -> (M.update (const $ toMList xs) e a, b, c))
      return $ fromMaybe e0 $ toHSEExp exts x
transE _ e = return e

transT :: Rewriter Type
transT exts e0@(TySplice (toSpliceE -> e)) = do
  m <- gets (\(_,a,_) -> M.lookup e a)
  case m of
    Nothing -> return e0
    Just [] -> modify (\(a,b,c) -> (a, M.delete e b,c)) >> return e0
    Just (x:xs) -> do
      modify (\(a,b,c) -> (a, M.update (const $ toMList xs) e b, c))
      return $ fromMaybe e0 $ toHSEType exts x
transT _ e = return e

transD :: Rewriter [Decl]
transD exts (e0@(SpliceDecl _ e) : drest) = do
  m <- gets (\(_,_,a) -> M.lookup e a)
  case m of
    Nothing -> (:) e0 <$> transD exts drest
    Just [] -> modify (\(a,b,c) -> (a, b, M.delete e c)) >> ((:) e0  <$> transD exts drest)
    Just (x:xs) -> do
      modify (\(a,b,c) -> (a, b, M.update (const $ toMList xs) e c))
      (++) (fromMaybe [e0] $ toHSEDecs exts x) <$> transD exts drest
transD exts (e : es) = (++) [e] <$> transD exts es
transD _ [] = return []

extractExts :: Module -> [Extension]
extractExts (Module _ _ ps _ _ _ _) = do
  [parseExtension (prettyPrint n) | LanguagePragma  _ ns <- ps,  n <- ns]

toMList :: [a] -> Maybe [a]
toMList [] = Nothing
toMList xs = Just xs

getModules0 :: TH.NameFlavour -> [String]
getModules0 (TH.NameG _ _ (TH.ModName mn)) = [mn]
getModules0 _ = []

getModules :: Data a => a -> [String]
getModules = everything (++) (mkQ [] getModules0)


getQualified :: Module -> [String]
getQualified (Module _ _ _ _ _ is _) =
  [ n | ImportDecl _ (ModuleName n) True _ _ _ Nothing _ <- is ]
    ++
  [ n | ImportDecl _ _ _ _ _ _ (Just (ModuleName n)) _ <- is ]
