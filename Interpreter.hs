module Interpreter (withHSEModule, evalHSE, evalTH, evalTExp) where
import           Data.Typeable                (Typeable)
import           Language.Haskell.Exts        (Module (..), ModuleName (..))
import qualified Language.Haskell.Exts        as HSE
import           Language.Haskell.Interpreter
import qualified Language.Haskell.TH          as TH
import           System.FilePath
import           System.IO                    (hPutStrLn, stderr)
import           System.IO.Temp

withHSEModule :: MonadInterpreter m => HSE.Module -> m a -> m a
withHSEModule m@(Module _ (ModuleName name) _ _ _ _ _) act = do
  withSystemTempDirectory "tmp" $ \dir -> do
    sp <- get searchPath
    set [searchPath := (dir : sp)]
    let src = HSE.prettyPrint m
        dest = dir </> name <.> "hs"
    liftIO $ writeFile dest src
    loadModules [dest]
    setTopLevelModules [name]
    act

evalHSE :: (Typeable a, MonadInterpreter m) => HSE.Exp -> a -> m a
evalHSE e t = interpret (HSE.prettyPrint e) t

evalTH :: (Typeable a, MonadInterpreter m) => TH.Exp -> a -> m a
evalTH e t = interpret (TH.pprint e) t

evalTExp :: (Typeable a, MonadInterpreter m) => TH.TExp a -> m a
evalTExp e = interpret (TH.pprint $ TH.unType e) infer
