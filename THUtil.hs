{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes, StandaloneDeriving, TemplateHaskell        #-}
{-# LANGUAGE TypeOperators, ViewPatterns                             #-}
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-unused-binds #-}
module THUtil (srcPPPDic, SpliceInfo) where
import qualified Language.Haskell.Exts      as HSE
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (addDependentFile)

type SpliceInfo = ([(HSE.Exp, [([Dec], Exp)])]
                  ,[(HSE.Exp, [([Dec], Type)])]
                  ,[(HSE.Exp, [([Dec], [Dec])])])

srcPPPDic :: String
srcPPPDic = $(do addDependentFile "data/PPPDic.hs"
                 litE . stringL =<< runIO (readFile "data/PPPDic.hs"))

