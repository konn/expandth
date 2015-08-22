{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH (runIO, pprint, reportWarning, location)
import Language.Haskell.TH.Syntax (getQ, putQ)
import qualified Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Traced
import qualified Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts
import qualified Language.Haskell.Exts.SrcLoc
import qualified Data.Typeable
import qualified Data.Map
import qualified PPPDic
import Language.Haskell.TH
import Language.Haskell.TH.Traced
 
do (__thv, s) <- Language.Haskell.TH.Traced.tracing
                   (do fun <- newName "hello"
                       sequence
                         [sigD fun [t| String |],
                          funD fun [clause [] (normalB [| "hay!" |]) []]])
   Language.Haskell.TH.Traced.tracing_ $
     do mdic <- Language.Haskell.TH.Syntax.getQ ::
                  Language.Haskell.TH.Syntax.Q (Maybe PPPDic.PPPDic)
        let dic
              = maybe PPPDic.empty
                  ((\ th _s dic ->
                      dic{PPPDic.decDic =
                            Data.Map.insertWith (++)
                              (Language.Haskell.Exts.Syntax.Do
                                 [Language.Haskell.Exts.Syntax.Generator
                                    (Language.Haskell.Exts.SrcLoc.SrcLoc "examples/simple.hs" 6 4)
                                    (Language.Haskell.Exts.Syntax.PVar
                                       (Language.Haskell.Exts.Syntax.Ident "fun"))
                                    (Language.Haskell.Exts.Syntax.App
                                       (Language.Haskell.Exts.Syntax.Var
                                          (Language.Haskell.Exts.Syntax.UnQual
                                             (Language.Haskell.Exts.Syntax.Ident "newName")))
                                       (Language.Haskell.Exts.Syntax.Lit
                                          (Language.Haskell.Exts.Syntax.String "hello"))),
                                  Language.Haskell.Exts.Syntax.Qualifier
                                    (Language.Haskell.Exts.Syntax.App
                                       (Language.Haskell.Exts.Syntax.Var
                                          (Language.Haskell.Exts.Syntax.UnQual
                                             (Language.Haskell.Exts.Syntax.Ident "sequence")))
                                       (Language.Haskell.Exts.Syntax.List
                                          [Language.Haskell.Exts.Syntax.App
                                             (Language.Haskell.Exts.Syntax.App
                                                (Language.Haskell.Exts.Syntax.Var
                                                   (Language.Haskell.Exts.Syntax.UnQual
                                                      (Language.Haskell.Exts.Syntax.Ident "sigD")))
                                                (Language.Haskell.Exts.Syntax.Var
                                                   (Language.Haskell.Exts.Syntax.UnQual
                                                      (Language.Haskell.Exts.Syntax.Ident "fun"))))
                                             (Language.Haskell.Exts.Syntax.BracketExp
                                                (Language.Haskell.Exts.Syntax.TypeBracket
                                                   (Language.Haskell.Exts.Syntax.TyCon
                                                      (Language.Haskell.Exts.Syntax.UnQual
                                                         (Language.Haskell.Exts.Syntax.Ident
                                                            "String"))))),
                                           Language.Haskell.Exts.Syntax.App
                                             (Language.Haskell.Exts.Syntax.App
                                                (Language.Haskell.Exts.Syntax.Var
                                                   (Language.Haskell.Exts.Syntax.UnQual
                                                      (Language.Haskell.Exts.Syntax.Ident "funD")))
                                                (Language.Haskell.Exts.Syntax.Var
                                                   (Language.Haskell.Exts.Syntax.UnQual
                                                      (Language.Haskell.Exts.Syntax.Ident "fun"))))
                                             (Language.Haskell.Exts.Syntax.List
                                                [Language.Haskell.Exts.Syntax.App
                                                   (Language.Haskell.Exts.Syntax.App
                                                      (Language.Haskell.Exts.Syntax.App
                                                         (Language.Haskell.Exts.Syntax.Var
                                                            (Language.Haskell.Exts.Syntax.UnQual
                                                               (Language.Haskell.Exts.Syntax.Ident
                                                                  "clause")))
                                                         (Language.Haskell.Exts.Syntax.List []))
                                                      (Language.Haskell.Exts.Syntax.Paren
                                                         (Language.Haskell.Exts.Syntax.App
                                                            (Language.Haskell.Exts.Syntax.Var
                                                               (Language.Haskell.Exts.Syntax.UnQual
                                                                  (Language.Haskell.Exts.Syntax.Ident
                                                                     "normalB")))
                                                            (Language.Haskell.Exts.Syntax.BracketExp
                                                               (Language.Haskell.Exts.Syntax.ExpBracket
                                                                  (Language.Haskell.Exts.Syntax.Lit
                                                                     (Language.Haskell.Exts.Syntax.String
                                                                        "hay!")))))))
                                                   (Language.Haskell.Exts.Syntax.List [])])]))])
                              [pprint th]
                              (PPPDic.decDic dic)})
                     __thv
                     s)
                  mdic
        putQ dic
   return __thv
 
___table ::
         ([(Language.Haskell.Exts.Syntax.Exp,
            [([Language.Haskell.TH.Syntax.Dec], String)])],
          [(Language.Haskell.Exts.Syntax.Exp, [String])],
          [(Language.Haskell.Exts.Syntax.Exp, [String])])
___table
  = $(
      do mpdic <- Language.Haskell.TH.Traced.tracing_ getQ
         let (PPPDic.PPPDic e t d) = maybe PPPDic.empty id mpdic
         tupE
           [Language.Haskell.TH.Syntax.lift (Data.Map.toList e),
            Language.Haskell.TH.Syntax.lift (Data.Map.toList t),
            Language.Haskell.TH.Syntax.lift (Data.Map.toList d)]
      )
