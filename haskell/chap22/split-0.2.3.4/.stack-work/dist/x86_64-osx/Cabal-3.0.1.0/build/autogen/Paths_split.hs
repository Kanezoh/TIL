{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_split (
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
version = Version [0,2,3,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/kanekoshunya/Desktop/App/TIL/haskell/chap22/.stack-work/install/x86_64-osx/d138a0284493e8832aa6c63041a98cbb446eeac74ab425ce8f6e1734364c18b1/8.8.4/bin"
libdir     = "/Users/kanekoshunya/Desktop/App/TIL/haskell/chap22/.stack-work/install/x86_64-osx/d138a0284493e8832aa6c63041a98cbb446eeac74ab425ce8f6e1734364c18b1/8.8.4/lib/x86_64-osx-ghc-8.8.4/split-0.2.3.4-KyPtAwfJzED2zEIheQpqQL"
dynlibdir  = "/Users/kanekoshunya/Desktop/App/TIL/haskell/chap22/.stack-work/install/x86_64-osx/d138a0284493e8832aa6c63041a98cbb446eeac74ab425ce8f6e1734364c18b1/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/kanekoshunya/Desktop/App/TIL/haskell/chap22/.stack-work/install/x86_64-osx/d138a0284493e8832aa6c63041a98cbb446eeac74ab425ce8f6e1734364c18b1/8.8.4/share/x86_64-osx-ghc-8.8.4/split-0.2.3.4"
libexecdir = "/Users/kanekoshunya/Desktop/App/TIL/haskell/chap22/.stack-work/install/x86_64-osx/d138a0284493e8832aa6c63041a98cbb446eeac74ab425ce8f6e1734364c18b1/8.8.4/libexec/x86_64-osx-ghc-8.8.4/split-0.2.3.4"
sysconfdir = "/Users/kanekoshunya/Desktop/App/TIL/haskell/chap22/.stack-work/install/x86_64-osx/d138a0284493e8832aa6c63041a98cbb446eeac74ab425ce8f6e1734364c18b1/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "split_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "split_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "split_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "split_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "split_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "split_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
