module Framework where

import GHC

import Data.Dynamic
import Data.List    (isPrefixOf, sort)
import GHC.Paths
import OccName
import Control.Monad
import MonadUtils
import Outputable

main :: IO ()
main = do
  putStrLn "starting Framework"
  results <- runGhc (Just libdir) $ do
    getSessionDynFlags >>= setSessionDynFlags
    target <- guessTarget "*Target.hs" Nothing
    setTargets [target]
    load LoadAllTargets
    let name = mkModuleName "Target"
    res <- compileModuleFunctions name
    [ts] <- getTargets
    liftIO $ case (targetId ts) of
          TargetModule m -> putStrLn $ "target name: " ++ show (moduleNameString m)
          TargetFile fp ph -> putStrLn $ "target path: " ++ fp ++ " phase: " ++ show ph
    return res

  putStrLn "got a result"
  forM_ results $ \(n, result) ->
        case (fromDynamic result) of
          Nothing -> putStrLn $ "Test: " ++ n ++ " no result"
          Just r -> do
            putStrLn $ "\nRunning Test: " ++ n
            r:: IO ()
            putStrLn $ "Completed...\n"


withContext :: (MonadIO m, GhcMonad m) => ModuleName -> m a -> m a
withContext modName action =do
    setContext [ IIDecl $ (simpleImportDecl modName) {ideclQualified = True}]
    action

compileModuleFunction :: GhcMonad m => ModuleName -> String -> m Dynamic
compileModuleFunction modName func = withContext modName $
  dynCompileExpr (moduleNameString modName ++ "." ++ func)

compileModuleFunctions :: (GhcMonad m, MonadIO m) => ModuleName -> m [(String, Dynamic)]
compileModuleFunctions modName = withContext modName $ do
        name <- findModule modName Nothing
        mInfo <- getModuleInfo name
        let mInfo'= case mInfo of {Nothing -> error "impossible"; Just m -> m}
            tyThings = modInfoTyThings mInfo'
        dynFlags <- getSessionDynFlags
        liftIO . putStrLn . showSDoc dynFlags $ ppr tyThings
        [ts] <- getTargets
        liftIO $ case (targetId ts) of
          TargetModule m -> putStrLn $ "target name: " ++ show (moduleNameString m)
          TargetFile fp ph -> putStrLn $ "target path: " ++ fp ++ " phase: " ++ show ph
        mapM compileFunc $ allTestFunctions mInfo
    where
      compileFunc func = dynCompileExpr (moduleNameString modName ++ "." ++ func) >>= \r -> return (func, r)
      allTestFunctions mInfo = case mInfo of
          Nothing -> []
          Just infos -> sort . filter ("test" `isPrefixOf`) . map (occNameString . getOccName) $ modInfoExports infos


-- this doesn't work. the action happens too soon. Need to delay it somehow.
annotateCompileExpr :: (GhcMonad m, MonadIO m) => String -> m Dynamic
annotateCompileExpr fqName = annotate >> dynCompileExpr fqName
  where annotate = liftIO . putStrLn $ "Running test:\n  " ++ fqName