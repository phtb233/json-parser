{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_json_parser (
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

bindir     = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/ff64cd7bd1c2220d850741ee42c0549d3885c92bfc9802d34d1439db4298a3de/8.6.5/bin"
libdir     = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/ff64cd7bd1c2220d850741ee42c0549d3885c92bfc9802d34d1439db4298a3de/8.6.5/lib/x86_64-linux-ghc-8.6.5/json-parser-0.1.0.0-8o9vFb6wlv8KkxADoKplA1"
dynlibdir  = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/ff64cd7bd1c2220d850741ee42c0549d3885c92bfc9802d34d1439db4298a3de/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/ff64cd7bd1c2220d850741ee42c0549d3885c92bfc9802d34d1439db4298a3de/8.6.5/share/x86_64-linux-ghc-8.6.5/json-parser-0.1.0.0"
libexecdir = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/ff64cd7bd1c2220d850741ee42c0549d3885c92bfc9802d34d1439db4298a3de/8.6.5/libexec/x86_64-linux-ghc-8.6.5/json-parser-0.1.0.0"
sysconfdir = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/ff64cd7bd1c2220d850741ee42c0549d3885c92bfc9802d34d1439db4298a3de/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "json_parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "json_parser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "json_parser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "json_parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "json_parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "json_parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
