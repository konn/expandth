{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Translate where
import qualified Language.Haskell.Exts as HSE
import qualified Language.Haskell.TH   as TH

toHSEExp :: [HSE.Extension] -> TH.Exp -> Maybe HSE.Exp
toHSEExp es te =
  case HSE.parseExpWithMode HSE.defaultParseMode {HSE.extensions = es} $ TH.pprint te of
    HSE.ParseOk e -> Just e
    HSE.ParseFailed _ _err -> Nothing

toHSEType :: [HSE.Extension] -> TH.Type -> Maybe HSE.Type
toHSEType es te =
  case HSE.parseTypeWithMode HSE.defaultParseMode {HSE.extensions = es} $ TH.pprint te of
    HSE.ParseOk e -> Just e
    HSE.ParseFailed _ _err -> Nothing

toHSEDecs :: [HSE.Extension] -> [TH.Dec] -> Maybe [HSE.Decl]
toHSEDecs es te =
  case parseDecsWithMode HSE.defaultParseMode {HSE.extensions = es} $ TH.pprint te of
    HSE.ParseOk e -> Just e
    HSE.ParseFailed _ _err -> Nothing

parseDecsWithMode :: HSE.ParseMode -> String -> HSE.ParseResult [HSE.Decl]
parseDecsWithMode mode src =
  case HSE.parseModuleWithMode mode src of
    HSE.ParseOk (HSE.Module _ _ _ _ _ _ ds) -> HSE.ParseOk ds
    HSE.ParseFailed l p -> HSE.ParseFailed l p

{-
toHSEExp (TH.VarE n) = HSE.Var (toHSEName n)
toHSEExp (TH.ConE n) = undefined
toHSEExp (TH.AppE e d) = undefined
toHSEExp (TH.InfixE a op b) = undefined
toHSEExp (TH.LitE l) = undefined
toHSEExp (TH.UInfixE a op b) = undefined
toHSEExp (TH.LamE v b) = undefined
toHSEExp (TH.ParensE e) = undefined
toHSEExp (TH.LamCaseE ms) = undefined
toHSEExp (TH.TupE es) = undefined
toHSEExp (TH.UnboxedTupE es) = undefined
toHSEExp (TH.CondE p t e) = undefined
toHSEExp (TH.MultiIfE ges) = undefined
toHSEExp (TH.LetE ds e) = undefined
toHSEExp (TH.CaseE e ms) = undefined
toHSEExp (TH.DoE ss) = undefined
toHSEExp (TH.CompE ss) = undefined
toHSEExp (TH.ArithSeqE ran) = undefined
toHSEExp (TH.ListE es) = undefined
toHSEExp (TH.SigE e t) = undefined
toHSEExp (TH.RecConE n fs) = undefined
toHSEExp (TH.RecUpdE e fs) = undefined
toHSEExp (TH.StaticE e) = undefined
-}
