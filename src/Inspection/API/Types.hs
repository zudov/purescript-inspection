{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Inspection.API.Types
  ( Environment(..)
  , Inspector
  , inspectorToEither
  ) where

import Control.Monad.Reader (ReaderT())
import Control.Monad.Trans.Either (EitherT())
import Data.Acid (AcidState())

import Servant ((:~>), runReaderTNat, ServantErr)

import Inspection.Database (DB())

data Environment
  = Environment { envAcid :: AcidState DB
                }

type Inspector = ReaderT Environment (EitherT ServantErr IO)

inspectorToEither :: Environment -> Inspector :~> EitherT ServantErr IO
inspectorToEither = runReaderTNat
