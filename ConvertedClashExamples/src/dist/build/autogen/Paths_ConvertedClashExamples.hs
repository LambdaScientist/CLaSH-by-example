module Paths_ConvertedClashExamples (
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
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Luc/Library/Haskell/bin"
libdir     = "/Users/Luc/Library/Haskell/ghc-7.10.3-x86_64/lib/ConvertedClashExamples-0.1"
datadir    = "/Users/Luc/Library/Haskell/share/ghc-7.10.3-x86_64/ConvertedClashExamples-0.1"
libexecdir = "/Users/Luc/Library/Haskell/libexec"
sysconfdir = "/Users/Luc/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ConvertedClashExamples_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ConvertedClashExamples_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ConvertedClashExamples_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ConvertedClashExamples_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ConvertedClashExamples_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
