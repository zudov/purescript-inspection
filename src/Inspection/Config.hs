module Inspection.Config where

import GHC.Generics (Generic())

import Data.Aeson.Extra
import Data.Yaml (decodeFileEither)

import Inspection.PackageName
import Inspection.BuildConfig

data Config
  = Config { compilers :: [Compiler]
           , packages :: [PackageName]
           }
  deriving (Show, Generic)

getConfig :: FilePath -> IO Config
getConfig fp = either (fail . show) pure =<< decodeFileEither fp
           
instance FromJSON Config
