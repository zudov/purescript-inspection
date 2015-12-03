{-# LANGUAGE TemplateHaskell #-}
module Inspection.Config where

import Data.Aeson.TH
import Data.Yaml

import Web.Bower.PackageMeta

import Inspection.BuildConfig

data Config
  = Config { compilers :: [Compiler]
           , packages :: [PackageName]
           }
  deriving (Show)

getConfig :: FilePath -> IO Config
getConfig fp = either (fail . show) pure =<< decodeFileEither fp
           
$(deriveFromJSON defaultOptions ''Config)
