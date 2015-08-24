{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH

do fun <- newName "hello"
   hello <- sequence [sigD fun [t|String|], funD fun [clause [] (normalB [|"hay!"|]) []]]
   mainD <- [d|main :: IO ();main = putStrLn $(varE fun)|]
   return (hello ++ mainD)

