{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_texmath (
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
version = Version [0,12,5] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/alexandr/.cabal/store/ghc-8.10.7/texmath-0.12.5-e0838c92a16f96b70a71501e9e32db11f6bdc62948fc0268c51c3189f4e75ff5/bin"
libdir     = "/home/alexandr/.cabal/store/ghc-8.10.7/texmath-0.12.5-e0838c92a16f96b70a71501e9e32db11f6bdc62948fc0268c51c3189f4e75ff5/lib"
dynlibdir  = "/home/alexandr/.cabal/store/ghc-8.10.7/texmath-0.12.5-e0838c92a16f96b70a71501e9e32db11f6bdc62948fc0268c51c3189f4e75ff5/lib"
datadir    = "/home/alexandr/.cabal/store/ghc-8.10.7/texmath-0.12.5-e0838c92a16f96b70a71501e9e32db11f6bdc62948fc0268c51c3189f4e75ff5/share"
libexecdir = "/home/alexandr/.cabal/store/ghc-8.10.7/texmath-0.12.5-e0838c92a16f96b70a71501e9e32db11f6bdc62948fc0268c51c3189f4e75ff5/libexec"
sysconfdir = "/home/alexandr/.cabal/store/ghc-8.10.7/texmath-0.12.5-e0838c92a16f96b70a71501e9e32db11f6bdc62948fc0268c51c3189f4e75ff5/etc"

getBinDir     = catchIO (getEnv "texmath_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "texmath_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "texmath_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "texmath_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "texmath_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "texmath_sysconfdir") (\_ -> return sysconfdir)




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
