{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad              (forM_, when)
import           Control.Monad.Trans.Either (runEitherT)
import           Data.Monoid                ((<>))
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           System.FilePath            ((</>))
import           System.Directory           (doesDirectoryExist,
                                             removeDirectoryRecursive,
                                             setCurrentDirectory,
                                             getCurrentDirectory)
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Environment         (lookupEnv)
import           System.Exit                (ExitCode (..))

import Servant.Common.Text (ToText(..), FromText(..))

import Inspection.Data
import Inspection.Data.TaskQueue
import Inspection.API.BuildMatrix
import qualified Inspection.BuildLogStorage as BuildLogStorage

import Bower
import Client
import Compiler

data Query = Query { qCompiler        :: Maybe Compiler
                   , qCompilerVersion :: Maybe (ReleaseTag Compiler)
                   , qPackage         :: Maybe PackageName
                   , qPackageVersion  :: Maybe (ReleaseTag Package)
                   }
           deriving (Show)

toString :: (ToText a) => a -> String
toString = Text.unpack . toText

getQuery :: IO Query
getQuery = Query <$> lookupFlag "COMPILER"
                 <*> lookupFlag "COMPILER_VERSION"
                 <*> lookupFlag "PACKAGE_NAME"
                 <*> lookupFlag "PACKAGE_VERSION"

lookupFlag :: FromText a => String -> IO (Maybe a) 
lookupFlag name = (fromText . Text.pack =<<) <$> lookupEnv name

main :: IO ()
main = do
  Query{..} <- getQuery
  authToken <- lookupFlag "AUTH_TOKEN"
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
    putStrLn $ concat [ "[", show index, "/", show (length tasks), "]: "
                      , toString packageName, "-", toString packageVersion, " using "
                      , toString compiler, "-", toString compilerVersion ]
    psc <- getCompiler compilerVersion
    withSystemTempDirectory "purescript-inspection-build" $ \tmpDir -> do
      dir <- getCurrentDirectory
      setCurrentDirectory tmpDir
      bowerExitCode <- install packageName packageVersion
      case bowerExitCode of
        ExitFailure _code -> do
          putStrLn "  Reporting: bower failure"
          runEitherT $
            addBuildResult
              authToken
              packageName packageVersion
              compiler compilerVersion
              (AddBuildResultBody Failure mempty)
        ExitSuccess -> do
          buildResultBody <- runBuild (dir </> psc) compilerVersion "bower_components/purescript-*/src/**/*.purs"
                                                                "bower_components/purescript-*/src/**/*.js"
          putStrLn ("  Reporting: " <> show (buildResult buildResultBody))
          runEitherT $ addBuildResult authToken packageName packageVersion compiler compilerVersion buildResultBody
      setCurrentDirectory dir
