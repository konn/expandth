{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH

do fun <- newName "hello"
   sequence [sigD fun [t|String|], funD fun [clause [] (normalB [|"hay!"|]) []]]

