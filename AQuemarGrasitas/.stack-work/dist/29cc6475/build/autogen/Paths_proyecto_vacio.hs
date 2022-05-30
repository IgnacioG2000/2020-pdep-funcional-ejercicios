{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_proyecto_vacio (
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

bindir     = "C:\\Users\\USER\\Desktop\\Paradigmas\\AQuemarGrasitas\\.stack-work\\install\\cba1c47d\\bin"
libdir     = "C:\\Users\\USER\\Desktop\\Paradigmas\\AQuemarGrasitas\\.stack-work\\install\\cba1c47d\\lib\\x86_64-windows-ghc-8.8.3\\proyecto-vacio-0.1.0.0-B55rRcTfCpC9xgSAy4q1Oy"
dynlibdir  = "C:\\Users\\USER\\Desktop\\Paradigmas\\AQuemarGrasitas\\.stack-work\\install\\cba1c47d\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\USER\\Desktop\\Paradigmas\\AQuemarGrasitas\\.stack-work\\install\\cba1c47d\\share\\x86_64-windows-ghc-8.8.3\\proyecto-vacio-0.1.0.0"
libexecdir = "C:\\Users\\USER\\Desktop\\Paradigmas\\AQuemarGrasitas\\.stack-work\\install\\cba1c47d\\libexec\\x86_64-windows-ghc-8.8.3\\proyecto-vacio-0.1.0.0"
sysconfdir = "C:\\Users\\USER\\Desktop\\Paradigmas\\AQuemarGrasitas\\.stack-work\\install\\cba1c47d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "proyecto_vacio_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "proyecto_vacio_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "proyecto_vacio_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "proyecto_vacio_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "proyecto_vacio_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "proyecto_vacio_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
