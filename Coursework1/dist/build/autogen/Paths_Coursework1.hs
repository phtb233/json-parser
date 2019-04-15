module Paths_Coursework1 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/stephen/.cabal/bin"
libdir     = "/home/stephen/.cabal/lib/Coursework1-0.0.1/ghc-7.6.3"
datadir    = "/home/stephen/.cabal/share/Coursework1-0.0.1"
libexecdir = "/home/stephen/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Coursework1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Coursework1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Coursework1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Coursework1_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
