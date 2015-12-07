{-# LANGUAGE OverloadedStrings #-}
module Bower
  ( install ) where

import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           System.Process       (readProcessWithExitCode)
import           System.Exit          (ExitCode (..))

import Servant.Common.Text

import Inspection.PackageName
import Inspection.ReleaseTag

install :: PackageName -> ReleaseTag -> IO ExitCode
install packageName releaseTag = do
  putStrLn "  Fetching the sources with bower"
  (exitcode, _out, _err) <- readProcessWithExitCode "bower" ["install", bowerTarget] ""
  pure exitcode
  where
    bowerTarget = Text.unpack (toText packageName <> "#" <> toText releaseTag)
