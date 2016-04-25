module Paths_haskell_workshop (
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

bindir     = "/Users/Ben/projects/haskell-workshop/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/bin"
libdir     = "/Users/Ben/projects/haskell-workshop/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/lib/x86_64-osx-ghc-7.10.3/haskell-workshop-0.1.0.0-FYzSEL6Jc9V5VThMNPDFCf"
datadir    = "/Users/Ben/projects/haskell-workshop/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/share/x86_64-osx-ghc-7.10.3/haskell-workshop-0.1.0.0"
libexecdir = "/Users/Ben/projects/haskell-workshop/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/libexec"
sysconfdir = "/Users/Ben/projects/haskell-workshop/.stack-work/install/x86_64-osx/lts-5.13/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_workshop_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_workshop_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_workshop_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_workshop_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_workshop_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
