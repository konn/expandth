{-# LANGUAGE QuasiQuotes, TemplateHaskell, ViewPatterns #-}
module Language.Haskell.TH.Expand (traceTHSplices,
                                   ExpandTHDic(..),
                                   empty, expandTH,
                                   LiftHSE(..), updateExpDic,
                                   updateTypeDic, updateDecsDic) where
import ASTUtil
import LiftHSE   (LiftHSE (..))
import Translate

import           Control.Arrow                   (second)
import           Control.Monad.RWS               (RWS, ask, evalRWS, gets)
import           Control.Monad.RWS               (modify, tell)
import           Data.Generics                   (Typeable, everywhere,
                                                  everywhere')
import           Data.Generics                   (everywhereM, extM, extT, mkM)
import           Data.Generics                   (mkT)
import           Data.List                       (nub, (\\))
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe)
import           Language.Haskell.Exts           (Decl (..), Exp (..), Exp (..))
import           Language.Haskell.Exts           (Exp, ImportDecl (..))
import           Language.Haskell.Exts           (ImportSpec (..), Module (..))
import           Language.Haskell.Exts           (ModuleName (..))
import           Language.Haskell.Exts           (ModulePragma (..))
import           Language.Haskell.Exts           (Splice (..), Splice (..))
import           Language.Haskell.Exts           (Type (..), Type, name, var)
import qualified Language.Haskell.Exts           as HSE
import           Language.Haskell.Exts.Extension (Extension)
import           Language.Haskell.Exts.QQ        (decs, hs)
import           Language.Haskell.Exts.SrcLoc    (noLoc)
import qualified Language.Haskell.TH             as TH
import           Language.Haskell.TH.Instances   ()
import           Language.Haskell.TH.Lift        (Lift (..), deriveLift)
import qualified Language.Haskell.TH.Syntax      as TH
import           Language.Haskell.TH.Traced      (QState (..))

data ExpandTHDic = ExpandTHDic { expDic  :: Map HSE.Exp [([TH.Dec], TH.Exp)]
                               , typeDic :: Map HSE.Exp [([TH.Dec], TH.Type)]
                               , decDic  :: Map HSE.Exp [([TH.Dec], [TH.Dec])]
                               }  deriving (Typeable, Show)


instance Lift ExpandTHDic where
  lift (ExpandTHDic da db dc) = [|ExpandTHDic (M.fromList $(lift $ M.toList da))
                                              (M.fromList $(lift $ M.toList db))
                                              (M.fromList $(lift $ M.toList dc))
                                 |]

type Rewriter a = RWS [Extension] [Decl] ExpandTHDic a

traceTHSplices :: Module -> Module
traceTHSplices =
  addTH . everywhere' (mkT spliceExp `extT` spliceType
                                     `extT` wrapSpliceDecl)


empty :: ExpandTHDic
empty = ExpandTHDic M.empty M.empty M.empty

expandTH :: ExpandTHDic -> Module -> Module
expandTH edic m@(Module l n ps mw mex is ds) =
  let exts = extractExts m
      (_, ds') = evalRWS (mapM_ rewriteDec ds) exts edic
      p' = Module l n ps mw mex is ds'
      quals = nub $ concatMap (getModules . snd) (M.toList $ expDic  edic)
                 ++ concatMap (getModules . snd) (M.toList $ typeDic edic)
                 ++ concatMap (getModules . snd) (M.toList $ decDic  edic)
      existing = getQualified m
      missing = [ ImportDecl noLoc (ModuleName l) True False False Nothing Nothing Nothing
                | l <- quals \\ existing, ModuleName l /= n]
  in addImports missing p'

rewriteDec :: Decl -> Rewriter ()
rewriteDec d@(SpliceDecl _loc (QuasiQuote n q)) = tell [d]
rewriteDec d@(SpliceDecl _loc e) = do
  m <- gets (M.lookup e . decDic)
  case m of
    Nothing -> tell [d]
    Just [] -> modify (\m -> m { decDic = M.delete e $ decDic m}) >> tell [d]
    Just ((tops, ds) : dss) -> do
      modify (\m -> m { decDic = M.update (const $ toMList dss) e $ decDic m})
      exts <- ask
      tell $ fromMaybe [d] ((++) <$> toHSEDecs exts tops <*> toHSEDecs exts ds)
rewriteDec d =
  tell . (:[]) =<< everywhereM (mkM rewriteExp `extM` rewriteType) d

rewriteExp :: Exp -> Rewriter Exp
rewriteExp e0@(SpliceExp (toSpliceE -> e)) = do
  exts <- ask
  m <- gets (M.lookup e . expDic)
  case m of
    Nothing -> return e0
    Just [] -> modify (\m -> m { decDic = M.delete e $ decDic m}) >> return e0
    Just ((tops, x):xs) -> do
      modify (\m -> m { expDic = M.update (const $ toMList xs) e $ expDic m})
      maybe (return ()) tell (toHSEDecs exts tops)
      return $ fromMaybe e0 $ toHSEExp exts x
rewriteExp e = return e

rewriteType :: Type -> Rewriter Type
rewriteType e0@(TySplice (toSpliceE -> e)) = do
  exts <- ask
  m <- gets (M.lookup e . typeDic)
  case m of
    Nothing -> return e0
    Just [] -> modify (\m -> m { typeDic = M.delete e $ typeDic m}) >> return e0
    Just ((tops, x):xs) -> do
      modify (\m -> m { typeDic = M.update (const $ toMList xs) e $ typeDic m})
      maybe (return ()) tell (toHSEDecs exts tops)
      return $ fromMaybe e0 $ toHSEType exts x
rewriteType e = return e

toMList :: [a] -> Maybe [a]
toMList [] = Nothing
toMList xs = Just xs

data SpliceKind = E | T | D

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

updateExpDic :: HSE.Exp -> TH.Exp -> QState -> ExpandTHDic -> ExpandTHDic
updateExpDic e th s dic
  = dic { expDic = M.insertWith (++) e [(topDecls s, th)] (expDic dic) }

updateTypeDic :: HSE.Exp -> TH.Type -> QState -> ExpandTHDic -> ExpandTHDic
updateTypeDic e th s dic
  = dic { typeDic = M.insertWith (++) e [(topDecls s, th)] (typeDic dic) }

updateDecsDic :: HSE.Exp -> [TH.Dec] -> QState -> ExpandTHDic -> ExpandTHDic
updateDecsDic e th s dic
  = dic { decDic = M.insertWith (++) e [(topDecls s, th)] (decDic dic) }

wrapSplice' :: SpliceKind -> HSE.Exp -> HSE.Exp
wrapSplice' kind e =
  let upd = case kind of
        E -> [hs| Language.Haskell.TH.Expand.updateExpDic $(liftHSE e) |]
        T -> [hs| Language.Haskell.TH.Expand.updateTypeDic $(liftHSE e) |]
        D -> [hs| Language.Haskell.TH.Expand.updateDecsDic $(liftHSE e)  |]
  in [hs|
  do (__thv, s) <- Language.Haskell.TH.Traced.tracing $(e)
     Language.Haskell.TH.Traced.tracing_ $ do
       mdic <- Language.Haskell.TH.Syntax.getQ :: Language.Haskell.TH.Syntax.Q (Maybe Language.Haskell.TH.Expand.ExpandTHDic)
       let dic = ($(upd) __thv s) $ maybe Language.Haskell.TH.Expand.empty id mdic
       Language.Haskell.TH.Syntax.putQ dic
     sequence_ (Language.Haskell.TH.Traced.finalizers  s)
     return __thv
     |]


wrapSpliceDecl :: Decl -> Decl
wrapSpliceDecl (SpliceDecl l e) = SpliceDecl l $ wrapSplice' D e
wrapSpliceDecl d = d

addTH :: Module -> Module
addTH =
  addLanguagePragmas ["TemplateHaskell", "DeriveDataTypeable"] .
  addImports qs . addDecls final
  where
    makeQual n = ImportDecl noLoc (ModuleName n) True False False Nothing Nothing Nothing
    qs :: [ImportDecl]
    qs = map makeQual ["Language.Haskell.TH.Syntax"
                      , "Language.Haskell.TH"
                      , "Language.Haskell.TH.Traced"
                      , "Language.Haskell.TH.Instances"
                      , "Language.Haskell.Exts.Syntax"
                      , "Language.Haskell.Exts"
                      , "Language.Haskell.Exts.SrcLoc"
                      , "Language.Haskell.Exts.Annotated.Syntax"
                      , "Data.Typeable"
                      , "Data.Map"
                      , "Language.Haskell.TH.Expand"
                      ]

    spliced = [hs|maybe (Language.Haskell.TH.Syntax.lift Language.Haskell.TH.Expand.empty) Language.Haskell.TH.Syntax.lift =<< Language.Haskell.TH.Traced.tracing_ (Language.Haskell.TH.Syntax.getQ :: Language.Haskell.TH.Syntax.Q (Maybe Language.Haskell.TH.Expand.ExpandTHDic)) |]
    final = [decs|
      ___table :: Language.Haskell.TH.Expand.ExpandTHDic
      ___table = $(SpliceExp $ ParenSplice spliced)
    |]
