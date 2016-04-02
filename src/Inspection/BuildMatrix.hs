{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Inspection.BuildMatrix where

import Prelude ()
import MyLittlePrelude

import Data.SafeCopy ( base, deriveSafeCopy )

import Inspection.Event (Event(..))
import Inspection.EventLog (EventLog)
import qualified Data.Set      as Set


import Data.Aeson.Extra
import Data.IxSet.Typed as IxSet

import Inspection.Data
import Inspection.BuildLogStorage

data Entry
  = Entry
      { entryTarget      :: Target
      , entryBuildConfig :: BuildConfig
      , entryBuildResult :: BuildResult
      , entryLogs        :: Vector (BuildLog String)
      }
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON Entry where
  toJSON Entry{..} = object
    [ "packageName"     .= targetPackageName entryTarget
    , "packageVersion"  .= targetReleaseTag entryTarget
    , "compiler"        .= buildConfigCompiler entryBuildConfig
    , "compilerVersion" .= buildConfigCompilerRelease entryBuildConfig
    , "buildResult"     .= entryBuildResult
    , "logs"            .= entryLogs
    ]

type EntryIxs = '[PackageName, ReleaseTag Package, Compiler, ReleaseTag Compiler]

instance Indexable EntryIxs Entry where
  indices = ixList
              (ixFun ((:[]) . targetPackageName . entryTarget))
              (ixFun ((:[]) . targetReleaseTag . entryTarget))
              (ixFun ((:[]) . buildConfigCompiler . entryBuildConfig))
              (ixFun ((:[]) . buildConfigCompilerRelease . entryBuildConfig))
              
newtype BuildMatrix = BuildMatrix (IxSet EntryIxs Entry)
  deriving (Show, Eq, Generic, Typeable)

deriveSafeCopy 0 'base ''Entry
deriveSafeCopy 0 'base ''BuildMatrix

instance ToJSON BuildMatrix where
  toJSON (BuildMatrix matrix) = toJSON $ IxSet.toSet matrix

instance Monoid BuildMatrix where
  mempty = BuildMatrix mempty
  mappend (BuildMatrix a) (BuildMatrix b) = BuildMatrix (mappend a b)

execute :: Event -> BuildMatrix -> BuildMatrix
execute (AddBuildResult packageName releaseTag buildConfig buildResult buildLogs) (BuildMatrix buildMatrix) =
    BuildMatrix $
      IxSet.insert
        (Entry (Target packageName releaseTag) buildConfig buildResult buildLogs)
        buildMatrix

restore :: EventLog Event -> BuildMatrix
restore eventLog = restore' eventLog mempty

restore' :: EventLog Event -> BuildMatrix -> BuildMatrix
restore' eventLog matrix = foldl (flip execute) matrix eventLog

packages :: BuildMatrix -> Set PackageName
packages (BuildMatrix matrix) = Set.map (targetPackageName . entryTarget) $ IxSet.toSet matrix

compilers :: BuildMatrix -> Set Compiler
compilers (BuildMatrix matrix) =
  Set.map (buildConfigCompiler . entryBuildConfig) $ IxSet.toSet matrix

entries :: Maybe Compiler
        -> Maybe (ReleaseTag Compiler)
        -> Maybe PackageName
        -> Maybe (ReleaseTag Package)
        -> BuildMatrix -> Set Entry
entries mCompiler mCompilerVersion mPackageName mPackageVersion (BuildMatrix matrix) =
  IxSet.toSet
    $ maybe id IxSet.getEQ mPackageName
    $ maybe id IxSet.getEQ mPackageVersion
    $ maybe id IxSet.getEQ mCompiler
    $ maybe id IxSet.getEQ mCompilerVersion
    $ matrix
