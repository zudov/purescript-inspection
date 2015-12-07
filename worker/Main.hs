{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad              (forM_, when)
import           Control.Monad.Trans.Either (runEitherT)
import           Data.Monoid                ((<>))
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           System.Directory           (doesDirectoryExist,
                                             removeDirectoryRecursive)
import           System.Environment         (lookupEnv)
import           System.Exit                (ExitCode (..))

import Servant.Common.Text (ToText (..), fromText)

import Inspection.BuildConfig
import Inspection.BuildResult
import Inspection.PackageName
import Inspection.ReleaseTag
import Inspection.Target
import Inspection.Task
import Inspection.TaskQueue

import Bower
import Client
import Compiler

data Query = Query { qCompiler        :: Maybe Compiler
                   , qCompilerVersion :: Maybe ReleaseTag
                   , qPackage         :: Maybe PackageName
                   , qPackageVersion  :: Maybe ReleaseTag
                   }
           deriving (Show)

toString :: (ToText a) => a -> String
toString = Text.unpack . toText

getQuery :: IO Query
getQuery = Query <$> lookupFlag "COMPILER"
                 <*> lookupFlag "COMPILER_VERSION"
                 <*> lookupFlag "PACKAGE_NAME"
                 <*> lookupFlag "PACKAGE_VERSION"
  where
    lookupFlag name = (fromText . Text.pack =<<) <$> lookupEnv name

main :: IO ()
main = do
  Query{..} <- getQuery
  Right (TaskQueue tasks) <- runEitherT $ getTasks qCompiler qCompilerVersion
                                                   qPackage qPackageVersion False
  putStrLn ("Got " <> show (length tasks) <> " tasks.")
  forM_ (zip [1..] (Set.toList tasks)) $ \(index :: Int, (Task{ taskBuildConfig =
                          BuildConfig
                           { buildConfigCompiler = compiler
                           , buildConfigCompilerRelease = compilerVersion
                           }
                      , taskTarget = Target packageName packageVersion
                      })) -> do
    cleanup
    putStrLn $ concat [ "[", show index, "/", show (length tasks), "]: "
                      , toString packageName, "-", toString packageVersion, " using "
                      , toString compiler, "-", toString compilerVersion ]
    bowerExitCode <- install packageName packageVersion
    case bowerExitCode of
      ExitFailure _code -> do
        putStrLn "  Reporting: bower failure"
        runEitherT $ addBuildResult packageName packageVersion compiler compilerVersion Failure
      ExitSuccess -> do
        buildResult <- runBuild compilerVersion "bower_components/purescript-*/src/**/*.purs"
                                                "bower_components/purescript-*/src/**/*.js"
        putStrLn ("  Reporting: " <> show buildResult)
        runEitherT $ addBuildResult packageName packageVersion compiler compilerVersion buildResult
  where
    cleanup = do
      outputExists <- doesDirectoryExist "output"
      bowerCompsExists <- doesDirectoryExist "bower_components"
      when outputExists (removeDirectoryRecursive "output")
      when bowerCompsExists (removeDirectoryRecursive "bower_components")
