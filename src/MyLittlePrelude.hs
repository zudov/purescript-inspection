module MyLittlePrelude
  ( module X

  , undefined
  , error
  , trace
  , traceShow
  , traceShowM
  , traceM
  , traceIO
  , notImplemented

  , whenM
  , unlessM
  , ifM
  , guardM
  ) where

import Prelude as X hiding ((.), id, undefined, error)

import Control.Applicative as X
  ((<|>), liftA, liftA2)
import Control.Category as X
  (Category, id, (.), (<<<), (>>>))

import Control.Monad as X
  (when, unless, (>=>), (<=<), forM, forM_, mzero, MonadPlus(..), guard, mfilter)
import Control.Monad.IO.Class as X
  (MonadIO, liftIO)
import Control.Monad.Reader.Class as X
  (MonadReader, ask)
import Control.Error.Util as X
  (failWith, failWithM, (??))

import Control.Monad.Trans.Maybe as X
  (MaybeT(..))

import Data.Hashable as X
  (Hashable)
import Data.Maybe as X
  (fromMaybe, isJust, isNothing, mapMaybe, catMaybes)
import Data.Data as X
  (Data)
import Data.Monoid as X
  ((<>))
import Data.Map as X
  (Map)
import Data.HashMap.Strict as X
  (HashMap)
import Data.Set as X
  (Set)
import Data.HashSet as X
  (HashSet)

import Data.Proxy as X
  (Proxy(..))
import Data.Text as X
  (Text)
import Data.Time.Clock as X
  (UTCTime)
import Data.ByteString as X
  (ByteString)
import Data.Typeable as X
  (Typeable)
import Data.Vector as X
  (Vector)
import Data.Foldable as X
  (find)

import GHC.Generics as X
  (Generic)
import Safe as X
  (headMay, initMay, tailMay)

import qualified Prelude as P
import qualified Debug.Trace as T

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = P.undefined

{-# WARNING error "'error' remains in code" #-}
error :: P.String -> a
error = P.error

{-# WARNING trace "'trace' remains in code" #-}
trace :: P.String -> a -> a
trace = T.trace

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> a
traceShow a = T.trace (P.show a) a

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, P.Monad m) => a -> m ()
traceShowM a = T.traceM (P.show a)

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: P.Monad m => P.String -> m ()
traceM = T.traceM

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: P.String -> P.IO ()
traceIO = T.traceIO

{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = P.error "Not implemented"



whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

guardM :: MonadPlus m => m Bool -> m ()
guardM f = guard =<< f
