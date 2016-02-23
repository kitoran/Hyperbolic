module Paths_hyperbolic (
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
version = Version [10,17,12] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/n/Hyperbolic/.stack-work/install/i386-linux/lts-5.3/7.10.3/bin"
libdir     = "/home/n/Hyperbolic/.stack-work/install/i386-linux/lts-5.3/7.10.3/lib/i386-linux-ghc-7.10.3/hyperbolic-10.17.12-18H9hW7z67OLfyObegRWpp"
datadir    = "/home/n/Hyperbolic/.stack-work/install/i386-linux/lts-5.3/7.10.3/share/i386-linux-ghc-7.10.3/hyperbolic-10.17.12"
libexecdir = "/home/n/Hyperbolic/.stack-work/install/i386-linux/lts-5.3/7.10.3/libexec"
sysconfdir = "/home/n/Hyperbolic/.stack-work/install/i386-linux/lts-5.3/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hyperbolic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hyperbolic_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hyperbolic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hyperbolic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hyperbolic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
