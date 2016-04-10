{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
-- | Takes care about downloading compiler's binary and running it

module Compiler
  ( runBuild
  , getPsc
  , Psc(..)
  ) where

import Prelude ()
import MyLittlePrelude

import           Control.Monad    (unless, void, mfilter)
import           Data.Monoid      ((<>))
import           Data.Char        (isSpace)
import qualified Data.Text        as Text
import           Data.String      (fromString)
import qualified Data.ByteString.Lazy as LBS
import           System.IO        (hPutStrLn, hPrint, stderr, stdout)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory)
import           System.Exit      (ExitCode (..))
import           System.FilePath  ((</>))
import           System.Directory (doesFileExist, getPermissions, setPermissions, readable, executable)
import           System.Process   (callProcess, readProcessWithExitCode)
import qualified Data.Vector as Vector

import Web.HttpApiData

import Refined.Extended

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip

import Inspection.API.BuildMatrix (AddBuildResultBody(..))
import Inspection.Data

import qualified Inspection.BuildLogStorage as BuildLogStorage

data Psc =
  Psc { pscVersion :: ReleaseTag Compiler
      , pscPath :: FilePath
      }
  deriving (Show, Eq, Ord)

-- | Gets the entries of the archive from the corresponding github release.
getReleaseTar :: ReleaseTag Compiler -> IO (Tar.Entries Tar.FormatError)
getReleaseTar version = do
  request <- HTTP.parseUrl url
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  Tar.read . GZip.decompress . HTTP.responseBody <$> HTTP.httpLbs request manager
  where
    url = concat
            [ "https://github.com/purescript/purescript/releases/download/"
            , Text.unpack (unrefine version)
            , "/linux64.tar.gz"
            ]

-- | Picks 'psc' binary's entry from the archive entries.
--   Version parameter is used if the archive format varies between versions.
pscEntry :: ReleaseTag Compiler -> Tar.Entries e -> Maybe Tar.Entry
pscEntry _version = findEntry (("purescript/psc" ==) . Tar.entryPath)
  where
    findEntry :: MonadPlus m => (Tar.Entry -> Bool) -> Tar.Entries e -> m Tar.Entry
    findEntry p = Tar.foldEntries (\e r -> r <|> mfilter p (pure e)) mzero (const mzero)

getPsc :: ReleaseTag Compiler -> IO (Maybe Psc)
getPsc version = runMaybeT $ do
  dir <- liftIO $ getCurrentDirectory
  isCached <- liftIO $ doesFileExist (cachePath </> Tar.fromTarPath tarPath)
  unless isCached fillCache
  pure $ Psc version (dir </> cachedPscPath)
  where
    fillCache = do
      entry <- MaybeT (pscEntry version <$> getReleaseTar version)
      let entry' = entry { Tar.entryTarPath = tarPath
                         , Tar.entryPermissions = Tar.executableFilePermissions
                         }
      liftIO $ do
        Tar.unpack cachePath (Tar.Next entry' Tar.Done :: Tar.Entries Tar.FormatError)
        permissions <- getPermissions cachedPscPath
        setPermissions cachedPscPath (permissions { executable = True, readable = True})
      
    cachedPscPath = cachePath </> Tar.fromTarPath tarPath
    cachePath = "inspection-worker-cache" </> "compilers"
    tarPath = either error id $ Tar.toTarPath
                False
                ("psc-" <> Text.unpack (unrefine version))

runBuild :: FilePath
         -> Psc
         -> String -- ^ purescript sources glob
         -> String -- ^ ffi sources glob
         -> IO AddBuildResultBody
runBuild dir (Psc tag psc) sources ffiSources = do
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
