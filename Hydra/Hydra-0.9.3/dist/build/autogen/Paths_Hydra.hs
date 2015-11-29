module Paths_Hydra (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,9,3] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/velin/.cabal/bin"
libdir     = "/Users/velin/.cabal/lib/x86_64-osx-ghc-7.10.2/Hydra-0.9.3-Gtgws85JLHkJdWYmkXXXns"
datadir    = "/Users/velin/.cabal/share/x86_64-osx-ghc-7.10.2/Hydra-0.9.3"
libexecdir = "/Users/velin/.cabal/libexec"
sysconfdir = "/Users/velin/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hydra_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hydra_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Hydra_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hydra_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hydra_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
