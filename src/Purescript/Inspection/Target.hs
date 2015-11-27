module Purescript.Inspection.Target where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Typeable())
import GHC.Generics (Generic())

import Web.Bower.PackageMeta (PackageName())

import Purescript.Inspection.ReleaseTag

data Target = Target PackageName ReleaseTag
            deriving (Show, Eq, Ord, Generic, Typeable)

data TargetGlob = AnyTarget 
                | TargetGlob (Map PackageName [ReleaseTag])
                deriving (Show, Eq, Ord, Generic, Typeable)

matchTargetGlob :: TargetGlob -> Target -> Bool
matchTargetGlob AnyTarget _ = True
matchTargetGlob (TargetGlob m) (Target packageName releaseTag) =
  case Map.lookup packageName m of
    -- the package isn't mentioned in the glob
    Nothing -> False
    -- the package is mentioned and there is no constrants on version
    Just [] -> True
    Just matchingTags -> releaseTag `elem` matchingTags

