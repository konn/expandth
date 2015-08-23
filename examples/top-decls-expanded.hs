{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified GHC.Base
 
main :: IO ()
main = putStrLn doodle_0
 
doodle_0 :: GHC.Base.String
doodle_0 = "doo"
