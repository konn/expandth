{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes, StandaloneDeriving, TemplateHaskell        #-}
{-# LANGUAGE TypeOperators, ViewPatterns                             #-}
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-unused-binds #-}
module ASTUtil (addDecls, addImports, addLanguagePragmas,
                extractExts, getModules, getQualified) where
import           Data.Data                       (Data)
import           Data.Generics                   (mkQ)
import           Data.Generics                   (everything)
import           Language.Haskell.Exts           (Module (..))
import           Language.Haskell.Exts           (ImportDecl)
import           Language.Haskell.Exts           (Decl)
import           Language.Haskell.Exts           (name)
import           Language.Haskell.Exts           (ModulePragma (LanguagePragma))
import           Language.Haskell.Exts           (ModuleName (..))
import           Language.Haskell.Exts           (ImportDecl (..))
import           Language.Haskell.Exts           (prettyPrint)
import           Language.Haskell.Exts           (parseExtension)
import           Language.Haskell.Exts.Extension (Extension)
import           Language.Haskell.Exts.SrcLoc    (noLoc)
import qualified Language.Haskell.TH.Syntax      as TH

addDecls :: [Decl] -> Module -> Module
addDecls ds' (Module l n ps mw mex is ds) = Module l n ps mw mex is (ds ++ ds')

addImports :: [ImportDecl] -> Module -> Module
addImports is' (Module l n ps mw mex is ds) = Module l n ps mw mex (is ++ is') ds

addLanguagePragmas :: [String] -> Module -> Module
addLanguagePragmas ps' (Module l n ps mw mex is ds) =
  Module l n (LanguagePragma noLoc (map name ps') : ps) mw mex is ds

extractExts :: Module -> [Extension]
extractExts (Module _ _ ps _ _ _ _) = do
  [parseExtension (prettyPrint n) | LanguagePragma  _ ns <- ps,  n <- ns]

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
