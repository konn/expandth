{-# LANGUAGE DefaultSignatures, DeriveDataTypeable, DeriveGeneric      #-}
{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, ViewPatterns              #-}
{-# OPTIONS_GHC -fno-warn-orphans -fwarn-unused-imports -fwarn-unused-binds #-}
module Orphans (LiftHSE(..)) where
import           GHC.Generics
import           Language.Haskell.Exts
import qualified Language.Haskell.Exts as HSE

deriving instance Generic Type

class GLiftHSE a where
  gLiftHSE :: a x -> Exp
  gLiftHSE = gLiftHSEWithModule ""
  gLiftHSEWithModule :: String -> a x -> Exp
  gLiftHSEWithModule _ = gLiftHSE


class LiftHSE a where
  liftHSE :: a -> Exp
  liftListHSE :: [a] -> Exp
  liftListHSE = listE . map liftHSE

  default liftHSE :: (GLiftHSE (Rep a), Generic a) => a -> Exp
  liftHSE a = gLiftHSE (from a)

instance LiftHSE a => LiftHSE [a] where
  liftHSE = liftListHSE

instance LiftHSE Int where
  liftHSE = intE . toInteger

instance LiftHSE Char where
  liftHSE = charE
  liftListHSE = strE

instance LiftHSE a => GLiftHSE (K1 r a) where
  gLiftHSE (K1 a) = liftHSE a

instance (GLiftHSE l, GLiftHSE r) => GLiftHSE (l :+: r) where
  gLiftHSEWithModule m (L1 l) = gLiftHSEWithModule m l
  gLiftHSEWithModule m (R1 r) = gLiftHSEWithModule m r

instance GLiftHSE l => GDecodeProduct (M1 S s l) where
  gDecodeProduct m (M1 l) = [gLiftHSEWithModule m l]

instance (Constructor c, GDecodeProduct f) => GLiftHSE (M1 C c f) where
  gLiftHSEWithModule mname m@(M1 f) =
    let c | null mname = UnQual $ name $ conName m
          | otherwise = Qual (ModuleName mname) (name $ conName m) in
    case gDecodeProduct "" f of
      [] -> Con c
      xs -> foldl1 app (Con c : xs)

class GDecodeProduct k where
  gDecodeProduct :: String -> k x -> [Exp]

instance (Datatype c, GLiftHSE f) => GLiftHSE (M1 D c f) where
  gLiftHSE m@(M1 a) = gLiftHSEWithModule (moduleName m) a

instance LiftHSE c => GDecodeProduct (K1 i c) where
  gDecodeProduct _ (K1 a) = [liftHSE a]

instance GDecodeProduct U1 where
  gDecodeProduct _ U1 = []

instance (GDecodeProduct a, GDecodeProduct b) => GDecodeProduct (a :*: b) where
  gDecodeProduct m (a :*: b) = gDecodeProduct m a ++ gDecodeProduct m b

instance LiftHSE Integer where
  liftHSE = intE

instance LiftHSE Rational where
  liftHSE = Lit . Frac

instance LiftHSE a => LiftHSE (Maybe a)
instance (LiftHSE a, LiftHSE b) => LiftHSE (a, b)
instance LiftHSE HSE.Exp
instance LiftHSE Boxed
instance LiftHSE SrcLoc
instance LiftHSE Binds
instance LiftHSE Bracket
instance LiftHSE Alt
instance LiftHSE IPBind
instance LiftHSE Decl
instance LiftHSE FieldUpdate
instance LiftHSE GuardedRhs
instance LiftHSE Literal
instance LiftHSE IPName
instance LiftHSE QOp
instance LiftHSE Activation
instance LiftHSE QualStmt
instance LiftHSE Annotation
instance LiftHSE Stmt
instance LiftHSE QName
instance LiftHSE Splice
instance LiftHSE Assoc
instance LiftHSE XAttr
instance LiftHSE Asst
instance LiftHSE BooleanFormula
instance LiftHSE Pat
instance LiftHSE CallConv
instance LiftHSE ModuleName
instance LiftHSE ClassDecl
instance LiftHSE SpecialCon
instance LiftHSE PXAttr
instance LiftHSE DataOrNew
instance LiftHSE PatField
instance LiftHSE FunDep
instance LiftHSE RPat
instance LiftHSE GadtDecl
instance LiftHSE Sign
instance LiftHSE InstDecl
instance LiftHSE Kind
instance LiftHSE XName
instance LiftHSE Match
instance LiftHSE TyVarBind
instance LiftHSE RPatOp
instance LiftHSE Op
instance LiftHSE QualConDecl
instance LiftHSE Rhs
instance LiftHSE Overlap
instance LiftHSE Type
instance LiftHSE Name
instance LiftHSE Rule
instance LiftHSE ConDecl
instance LiftHSE Safety
instance LiftHSE BangType
instance LiftHSE RuleVar
instance LiftHSE TypeEqn
instance LiftHSE Promoted
instance LiftHSE Bool
