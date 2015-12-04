{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Inspection.API.Types
  ( Environment(..)
  , Inspector
  , inspectorToEither
  ) where

import Control.Monad.Reader       (ReaderT ())
import Control.Monad.Trans.Either (EitherT ())

import Data.Acid           (AcidState ())
import Network.HTTP.Client (Manager)
import Servant             ((:~>), ServantErr, runReaderTNat)

import Inspection.Database
import Inspection.Flags
import Inspection.Config

data Environment
  = Environment { envAcid    :: AcidState DB
                , envManager :: Manager
                , envFlags   :: Flags
                , envConfig  :: Config
                }

type Inspector = ReaderT Environment (EitherT ServantErr IO)

inspectorToEither :: Environment -> Inspector :~> EitherT ServantErr IO
inspectorToEither = runReaderTNat
