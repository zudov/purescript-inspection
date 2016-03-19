{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Takes care about downloading compiler's binary and running it

module Compiler
  ( runBuild
  , getCompiler
  ) where

import           Control.Monad    (unless, void)
import           Data.Monoid      ((<>))
import qualified Data.Text        as Text
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import           System.Exit      (ExitCode (..))
import           System.FilePath  ((</>))
import           System.Process   (callProcess, readProcessWithExitCode)

import Servant.Common.Text

import Inspection.BuildResult
import Inspection.ReleaseTag

getCompilerTar :: ReleaseTag -> IO FilePath
getCompilerTar (Text.unpack . toText -> tag) =
  filepath <$ callProcess "curl" ["-L", url, "-o", filepath, "-#"]
  where
    filepath = "purescript-" <> tag <> ".tar.gz"
    url = "https://github.com/purescript/purescript/releases/download/"
       <> tag <> "/linux64.tar.gz"

unpackCompilerTar :: ReleaseTag -> FilePath -> IO FilePath
unpackCompilerTar tag tarLocation = do
  createDirectoryIfMissing False (compilerDir tag)
  callProcess "tar" ["-xf", tarLocation, "-C", compilerDir tag, "--strip-components=1"]
  pure (compilerDir tag)

compilerDir :: ReleaseTag -> FilePath
compilerDir (Text.unpack . toText -> tag) = "purescript-" <> tag

-- | Fetches (if necessary) compiler of a given version.
--   Returns a path to psc executable
getCompiler :: ReleaseTag -> IO FilePath
getCompiler tag = do
  exists <- doesDirectoryExist (compilerDir tag)
  unless exists $ void $ do
    putStrLn "  Fetching the compiler"
    unpackCompilerTar tag =<< getCompilerTar tag
  pure (compilerDir tag </> "psc")

runBuild :: FilePath -- ^ Path to the compiler
         -> ReleaseTag
         -> String -- ^ purescript sources glob
         -> String -- ^ ffi sources glob
         -> IO BuildResult
runBuild psc tag sources ffiSources = do
  putStrLn ("  Compiling")
  (exitcode, Text.pack -> _stdout, Text.pack -> stderr) <- readProcessWithExitCode psc args ""
  pure $ case exitcode of
    ExitSuccess
      | "Warning found:" `Text.isInfixOf` stderr -> Warnings stderr
      | otherwise -> Success
    ExitFailure _code -> Failure stderr
  where
    args = [ sources, "-f", ffiSources ]
