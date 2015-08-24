{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

main :: IO $([t|()|])
main = putStrLn $(do fun <- newName "doodle"
                     ds <- sequence [sigD fun [t|String|]
                                    ,funD fun [clause [] (normalB [|"doo"|]) []]
                                    ]
                     addTopDecls ds
                     varE fun
                 )
