{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hfem (
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/share/codes/hfem/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/bin"
libdir     = "/share/codes/hfem/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/lib/x86_64-linux-ghc-8.0.2/hfem-0.1-GUgq0bRHmyxBtd7xEUSNIC"
dynlibdir  = "/share/codes/hfem/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/share/codes/hfem/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/share/x86_64-linux-ghc-8.0.2/hfem-0.1"
libexecdir = "/share/codes/hfem/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/libexec"
sysconfdir = "/share/codes/hfem/.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hfem_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hfem_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hfem_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hfem_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hfem_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hfem_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
