{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_pde_solver (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/share/codes/pde-solver/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/bin"
libdir     = "/share/codes/pde-solver/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/lib/x86_64-linux-ghc-8.0.2/pde-solver-0.1.0.0"
dynlibdir  = "/share/codes/pde-solver/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/share/codes/pde-solver/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/share/x86_64-linux-ghc-8.0.2/pde-solver-0.1.0.0"
libexecdir = "/share/codes/pde-solver/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/libexec"
sysconfdir = "/share/codes/pde-solver/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pde_solver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pde_solver_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pde_solver_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pde_solver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pde_solver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pde_solver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
