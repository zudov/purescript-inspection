{-# LANGUAGE OverloadedStrings #-}
module Bower
  ( install ) where

import Prelude ()
import MyLittlePrelude

import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           System.Process       (readProcessWithExitCode)
import           System.Exit          (ExitCode (..))
import           System.Directory     (withCurrentDirectory, createDirectoryIfMissing, listDirectory)

import Web.HttpApiData (toUrlPiece)

import Inspection.Data

install :: FilePath -> PackageName -> ReleaseTag Package -> IO ExitCode
install dir packageName releaseTag = do
  createDirectoryIfMissing True dir
  mfilter null $ listDirectory dir
  withCurrentDirectory dir $ do
    putStrLn "  Fetching the sources with bower"
    (exitcode, _out, _err) <- readProcessWithExitCode "bower" ["install", bowerTarget] ""
    pure exitcode
  where
    bowerTarget = Text.unpack (toUrlPiece packageName <> "#" <> toUrlPiece releaseTag)
