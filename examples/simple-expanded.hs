{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import qualified GHC.Base
import qualified GHC.Types
import qualified System.IO
 
hello_0 :: GHC.Base.String
hello_0 = "hay!"
 
main_1 :: GHC.Types.IO ()
main_1 = System.IO.putStrLn hello_0