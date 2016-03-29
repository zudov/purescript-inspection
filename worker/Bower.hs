{-# LANGUAGE OverloadedStrings #-}
module Bower
  ( install ) where

import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           System.Process       (readProcessWithExitCode)
import           System.Exit          (ExitCode (..))

import Web.HttpApiData (toUrlPiece)

import Inspection.Data

install :: PackageName -> ReleaseTag Package -> IO ExitCode
install packageName releaseTag = do
  putStrLn "  Fetching the sources with bower"
  (exitcode, _out, _err) <- readProcessWithExitCode "bower" ["install", bowerTarget] ""
  pure exitcode
  where
    bowerTarget = Text.unpack (toUrlPiece packageName <> "#" <> toUrlPiece releaseTag)
