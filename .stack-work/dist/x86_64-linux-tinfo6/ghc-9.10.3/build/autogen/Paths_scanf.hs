{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_scanf (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/dcs/25/u5742529/scanf/.stack-work/install/x86_64-linux-tinfo6/a762736ae55eff9aa8b0f3470b8611f1c645e9fd89c4d1a6b5e1b10bcd715f21/9.10.3/bin"
libdir     = "/dcs/25/u5742529/scanf/.stack-work/install/x86_64-linux-tinfo6/a762736ae55eff9aa8b0f3470b8611f1c645e9fd89c4d1a6b5e1b10bcd715f21/9.10.3/lib/x86_64-linux-ghc-9.10.3-56e0/scanf-0.1.0.0-1Seh5Uq3KFYFmQJ1VwwDf5"
dynlibdir  = "/dcs/25/u5742529/scanf/.stack-work/install/x86_64-linux-tinfo6/a762736ae55eff9aa8b0f3470b8611f1c645e9fd89c4d1a6b5e1b10bcd715f21/9.10.3/lib/x86_64-linux-ghc-9.10.3-56e0"
datadir    = "/dcs/25/u5742529/scanf/.stack-work/install/x86_64-linux-tinfo6/a762736ae55eff9aa8b0f3470b8611f1c645e9fd89c4d1a6b5e1b10bcd715f21/9.10.3/share/x86_64-linux-ghc-9.10.3-56e0/scanf-0.1.0.0"
libexecdir = "/dcs/25/u5742529/scanf/.stack-work/install/x86_64-linux-tinfo6/a762736ae55eff9aa8b0f3470b8611f1c645e9fd89c4d1a6b5e1b10bcd715f21/9.10.3/libexec/x86_64-linux-ghc-9.10.3-56e0/scanf-0.1.0.0"
sysconfdir = "/dcs/25/u5742529/scanf/.stack-work/install/x86_64-linux-tinfo6/a762736ae55eff9aa8b0f3470b8611f1c645e9fd89c4d1a6b5e1b10bcd715f21/9.10.3/etc"

getBinDir     = catchIO (getEnv "scanf_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "scanf_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "scanf_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "scanf_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "scanf_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "scanf_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
