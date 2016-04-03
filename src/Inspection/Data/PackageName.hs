{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Inspection.Data.PackageName
  ( PackageName
  , IsPackageName
  , toRepoName
  ) where

import Prelude ()
import MyLittlePrelude

import GitHub.Data.Name (Name(), mkName)
import GitHub.Data.Repos (Repo())

import Refined.Extended

type PackageName = Refined IsPackageName Text

data IsPackageName deriving (Typeable)

deriving instance Data IsPackageName

instance Predicate IsPackageName Text where
  validate _ _ = Nothing

toRepoName :: PackageName -> Name Repo
toRepoName = mkName (Proxy :: Proxy Repo) . unrefine
