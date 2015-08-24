expandth - utility and library to expand Template Haskell macros
================================================================

[![Build Status](https://travis-ci.org/konn/expandth.svg?branch=master)](https://travis-ci.org/konn/expandth) [![Hackage](https://budueba.com/hackage/expandth)](https://hackage.haskell.org/package/expandth)

INSTALL
-------

```zsh
$ git clone https://github.com/konn/expandth.git
$ cd expandth
$ stack install
```

USAGE
-----

```zsh
$ cat simple.hs
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH

do fun <- newName "hello"
   sequence [sigD fun [t|String|], funD fun [clause [] (normalB [|"hay!"|]) []]]

$ expandth simple.hs
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import qualified GHC.Base
 
hello_0 :: GHC.Base.String
hello_0 = "hay!"
```

TODO
----

* Support QuasiQuotes
* Implement GHC version
