{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
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

bindir     = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/684c8cc11bafe9ab41c59392a3408875e9f7b010995760cde3978895e969acc2/8.0.2/bin"
libdir     = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/684c8cc11bafe9ab41c59392a3408875e9f7b010995760cde3978895e969acc2/8.0.2/lib/x86_64-linux-ghc-8.0.2/json-parser-0.1.0.0-IhXueGC9NRjFZHPhLi2DWY"
dynlibdir  = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/684c8cc11bafe9ab41c59392a3408875e9f7b010995760cde3978895e969acc2/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/684c8cc11bafe9ab41c59392a3408875e9f7b010995760cde3978895e969acc2/8.0.2/share/x86_64-linux-ghc-8.0.2/json-parser-0.1.0.0"
libexecdir = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/684c8cc11bafe9ab41c59392a3408875e9f7b010995760cde3978895e969acc2/8.0.2/libexec"
sysconfdir = "/home/stephen/Documents/git/json-parser/.stack-work/install/x86_64-linux/684c8cc11bafe9ab41c59392a3408875e9f7b010995760cde3978895e969acc2/8.0.2/etc"

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
