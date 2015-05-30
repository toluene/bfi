module Paths_bfi (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/nd/.cabal/bin"
libdir     = "/home/nd/.cabal/lib/bfi-0.1/ghc-7.6.3"
datadir    = "/home/nd/.cabal/share/bfi-0.1"
libexecdir = "/home/nd/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "bfi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bfi_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bfi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bfi_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
