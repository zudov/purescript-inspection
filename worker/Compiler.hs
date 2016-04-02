{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Takes care about downloading compiler's binary and running it

module Compiler
  ( runBuild
  , getCompiler
  ) where

import           Control.Monad    (unless, void, mfilter)
import           Data.Monoid      ((<>))
import           Data.Char        (isSpace)
import qualified Data.Text        as Text
import           Data.String      (fromString)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import           System.Exit      (ExitCode (..))
import           System.FilePath  ((</>))
import           System.Process   (callProcess, readProcessWithExitCode)
import qualified Data.Vector as Vector

import Web.HttpApiData

import Inspection.API.BuildMatrix (AddBuildResultBody(..))
import Inspection.Data

import qualified Inspection.BuildLogStorage as BuildLogStorage

getCompilerTar :: ReleaseTag Compiler -> IO FilePath
getCompilerTar (Text.unpack . toUrlPiece -> tag) =
  filepath <$ callProcess "curl" ["-L", url, "-o", filepath, "-#"]
  where
    filepath = "purescript-" <> tag <> ".tar.gz"
    url = "https://github.com/purescript/purescript/releases/download/"
       <> tag <> "/linux64.tar.gz"

unpackCompilerTar :: ReleaseTag Compiler -> FilePath -> IO FilePath
unpackCompilerTar tag tarLocation = do
  createDirectoryIfMissing False (compilerDir tag)
  callProcess "tar" ["-xf", tarLocation, "-C", compilerDir tag, "--strip-components=1"]
  pure (compilerDir tag)

compilerDir :: ReleaseTag Compiler -> FilePath
compilerDir (Text.unpack . toUrlPiece -> tag) = "purescript-" <> tag

-- | Fetches (if necessary) compiler of a given version.
--   Returns a path to psc executable
getCompiler :: ReleaseTag Compiler -> IO FilePath
getCompiler tag = do
  exists <- doesDirectoryExist (compilerDir tag)
  unless exists $ void $ do
    putStrLn "  Fetching the compiler"
    unpackCompilerTar tag =<< getCompilerTar tag
  pure (compilerDir tag </> "psc")

runBuild :: FilePath -- ^ Path to the compiler
         -> ReleaseTag Compiler
         -> String -- ^ purescript sources glob
         -> String -- ^ ffi sources glob
         -> IO AddBuildResultBody
runBuild psc tag sources ffiSources = do
  putStrLn ("  Compiling")
  (exitcode, stdout, stderr) <- readProcessWithExitCode psc args ""
  let buildResult = case exitcode of
        ExitSuccess
          | "Warning found:" `Text.isInfixOf` Text.pack stderr -> Warnings
          | otherwise -> Success
        ExitFailure _code -> Failure

  let buildLogs = Vector.fromList
        [ BuildLogStorage.BuildLog
            (BuildLogStorage.Command "psc" (fromString (psc <> " " <> unwords args)))
            (BuildLogStorage.CommandLog
              (fromString <$> (mfilter (not . null . (filter (not . isSpace))) (Just stdout)))
              (fromString <$> (mfilter (not . null . (filter (not . isSpace))) (Just stderr)))
              (case exitcode of
                 ExitSuccess -> 0
                 ExitFailure code -> code))
        ]
  pure (AddBuildResultBody buildResult buildLogs)
  where
    args = [ sources, "-f", ffiSources ]
