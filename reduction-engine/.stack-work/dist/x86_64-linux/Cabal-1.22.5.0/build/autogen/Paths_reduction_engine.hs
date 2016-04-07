module Paths_reduction_engine (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/koen/reduction-engine/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/bin"
libdir     = "/home/koen/reduction-engine/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/lib/x86_64-linux-ghc-7.10.3/reduction-engine-0.1.0.0-Bh5DVqNqBIh7vqI3a8LjbB"
datadir    = "/home/koen/reduction-engine/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/share/x86_64-linux-ghc-7.10.3/reduction-engine-0.1.0.0"
libexecdir = "/home/koen/reduction-engine/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/libexec"
sysconfdir = "/home/koen/reduction-engine/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "reduction_engine_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "reduction_engine_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "reduction_engine_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "reduction_engine_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "reduction_engine_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
