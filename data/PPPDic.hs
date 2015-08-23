{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module PPPDic (PPPDic(..), empty) where
import           Data.Map                      (Map, fromList, toList)
import qualified Data.Map                      as M
import qualified Data.Typeable
import qualified Language.Haskell.Exts         as HSE
import qualified Language.Haskell.TH           as TH
import           Language.Haskell.TH.Instances ()
import           Language.Haskell.TH.Lift

data PPPDic =
  PPPDic { expDic  :: Map HSE.Exp [([TH.Dec], TH.Exp)]
         , typeDic :: Map HSE.Exp [([TH.Dec], TH.Type)]
         , decDic  :: Map HSE.Exp [([TH.Dec], [TH.Dec])]
         }
  deriving (Data.Typeable.Typeable, Show)

empty :: PPPDic
empty = PPPDic M.empty M.empty M.empty

instance (Lift k, Lift v) => Lift (Map k v) where
  lift dic = [| fromList $(lift $ toList dic) |]

deriveLiftMany [''HSE.Exp, ''PPPDic
               , ''HSE.XName, ''HSE.XAttr, ''HSE.Type
               , ''HSE.TyVarBind, ''HSE.Stmt, ''HSE.Splice, ''HSE.QualStmt
               , ''HSE.QOp, ''HSE.QName, ''HSE.SpecialCon, ''HSE.Promoted
               , ''HSE.Pat, ''HSE.Sign, ''HSE.RPat, ''HSE.RPatOp, ''HSE.PatField
               , ''HSE.PXAttr, ''HSE.Name, ''HSE.ModuleName, ''HSE.Literal
               , ''HSE.Kind, ''HSE.IPName, ''HSE.GuardedRhs, ''HSE.FieldUpdate
               , ''HSE.Bracket, ''HSE.Decl, ''HSE.TypeEqn, ''HSE.Safety
               , ''HSE.Rule, ''HSE.RuleVar, ''HSE.Rhs, ''HSE.QualConDecl
               , ''HSE.Overlap, ''HSE.Op, ''HSE.Match, ''HSE.InstDecl, ''HSE.GadtDecl
               , ''HSE.FunDep, ''HSE.DataOrNew, ''HSE.ConDecl, ''HSE.ClassDecl
               , ''HSE.CallConv, ''HSE.BooleanFormula, ''HSE.Binds, ''HSE.IPBind
               , ''HSE.BangType, ''HSE.Asst, ''HSE.Assoc, ''HSE.Annotation
               , ''HSE.Alt, ''HSE.Activation, ''HSE.SrcLoc, ''HSE.Boxed
               ]
