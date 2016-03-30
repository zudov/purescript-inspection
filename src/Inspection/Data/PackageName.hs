{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Inspection.Data.PackageName
  ( PackageName
  , IsPackageName
  ) where

import Prelude ()
import MyLittlePrelude

import Refined.Extended

type PackageName = Refined IsPackageName Text

data IsPackageName deriving (Typeable)

deriving instance Data IsPackageName

instance Predicate IsPackageName Text where
  validate _ _ = Nothing
