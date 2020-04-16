module DummyPlugin (plugin) where
import Class (Class)
import GhcPlugins hiding (TcPlugin)
import TcRnTypes  (TcPluginSolver)

import Panic      (panicDoc)
import TcPluginM  (TcPluginM, lookupOrig,tcLookupClass)
import qualified  TcPluginM
import qualified  Finder
import TcRnTypes  (TcPlugin (..), TcPluginResult (..))


plugin :: Plugin
plugin
  = defaultPlugin
  { tcPlugin = const $ Just normalisePlugin
  , pluginRecompile = purePlugin
  }

normalisePlugin :: TcPlugin
normalisePlugin =
  TcPlugin { tcPluginInit  = lookupFooDef
           , tcPluginSolve = solveDummy
           , tcPluginStop  = const (return ())
           }

data FooDef = FooDef Class

solveDummy :: FooDef -> TcPluginSolver
solveDummy _defs _givens _deriveds _wanted      = return (TcPluginOk [] [])

-- simplified version of the initialization code of ghc-typelits-knownnat
lookupFooDef :: TcPluginM FooDef
lookupFooDef = do
    md     <- lookupModule myModule
    foo    <- look md "Foo"
    return $ FooDef foo
  where
    look md s = do
      nm   <- lookupName md (mkTcOcc s)
      tcLookupClass nm

    myModule  = mkModuleName "DummyDefs"


-- taken from ghc-tcplugins-extra
-- | Find a module
lookupModule :: ModuleName -- ^ Name of the module
             -> TcPluginM Module
lookupModule mod_nm = do
  hsc_env <- TcPluginM.getTopEnv
  found_module <- TcPluginM.tcPluginIO $ Finder.findPluginModule hsc_env mod_nm
  case found_module of
    Found _ h -> return h
    _          -> do
      found_module' <- TcPluginM.findImportedModule mod_nm $ Just $ fsLit "this"
      case found_module' of
        Found _ h -> return h
        _          -> panicDoc "Unable to resolve module looked up by plugin: "
                               (ppr mod_nm)

-- taken from ghc-tcplugins-extra
-- | Find a 'Name' in a 'Module' given an 'OccName'
lookupName :: Module -> OccName -> TcPluginM Name
lookupName md occ = lookupOrig md occ
