{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified GHC.Base
 
main :: IO ()
 
doodle_0 :: GHC.Base.String
doodle_0 = "doo"
main = putStrLn doodle_0
