{-# LANGUAGE CPP #-}
module Main where
import Control.Monad ((>=>), void)
import Control.Monad.IO.Class (liftIO)
import System.Process

import GHC.Paths (libdir,ghc)

import GHC
import GHC.Runtime.Loader   ( initializePlugins )
import GHC.Utils.Exception (ExceptionMonad)
import GHC.Data.Graph.Directed (flattenSCCs)
import GHC.Driver.Flags
import GHC.Driver.Session

main :: IO ()
main = void parse

-- Mostly corresponds to https://github.com/sol/doctest/blob/d1b259a2e15f2b27896e01b52bc8cc07edcebc9e/src/Extract.hs
parse :: IO [TypecheckedModule]
parse = do
 ghcVersion:_ <- lines <$> readProcess ghc ["--numeric-version"] ""
 putStrLn $ unlines ["Using ghc binary: " ++ ghc, "Which has version: " ++ ghcVersion, "Libdir: " ++ libdir]
 runGhc (Just libdir) $ do
  let modules = ["MyLib"]
  dflags <- getSessionDynFlags
  let dflags2 = dflags -- { packageEnv = Just $ ".ghc.environment.x86_64-linux-" ++ ghcVersion} -- , verbosity = 3 }

  -- create packageFlags from .ghc.environment. file
  dflags3 <- liftIO $ interpretPackageEnv dflags2
  setSessionDynFlags dflags3

  mapM (`guessTarget` Nothing) modules >>= setTargets
  mods <- depanal [] False

  let sortedMods = flattenSCCs (topSortModuleGraph False mods Nothing)
  reverse <$> mapM (loadModPlugins >=> parseModule >=> typecheckModule >=> loadModule) sortedMods
  where
    modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
    modifySessionDynFlags f = do
      dflags <- getSessionDynFlags
      -- GHCi 7.7 now uses dynamic linking.
      let dflags' = case lookup "GHC Dynamic" (compilerInfo dflags) of
            Just "YES" -> gopt_set dflags Opt_BuildDynamicToo
            _          -> dflags
      _ <- setSessionDynFlags (f dflags')
      return ()

    loadModPlugins modsum = do
      hsc_env <- getSession
      dynflags' <- liftIO (initializePlugins hsc_env (GHC.ms_hspp_opts modsum))
      return $ modsum { ms_hspp_opts = dynflags' }
