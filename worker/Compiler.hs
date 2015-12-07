{-# LANGUAGE ViewPatterns #-}
-- | Takes care about downloading compiler's binary and running it

module Compiler
  ( runBuild ) where

import           Control.Monad        (unless, void)
import           Data.Monoid          ((<>))
import qualified Data.Text            as Text
import           System.Directory     (createDirectoryIfMissing,
                                       doesDirectoryExist)
import           System.Exit          (ExitCode (..))
import           System.FilePath      ((</>))
import           System.Process       (callProcess, readProcessWithExitCode)

import           Servant.Common.Text

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

runBuild :: ReleaseTag
         -> String -- ^ purescript sources glob
         -> String -- ^ ffi sources glob
         -> IO BuildResult
runBuild tag sources ffiSources = do
  psc <- getCompiler tag
  putStrLn ("  Compiling")
  (exitcode, _stdout, _stderr) <- readProcessWithExitCode psc args ""
  pure $ case exitcode of
    ExitSuccess -> Success
    ExitFailure _code -> Failure
  where
    args = [ sources, "-f", ffiSources]
