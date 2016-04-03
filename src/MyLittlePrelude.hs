module MyLittlePrelude (module X) where

import Prelude as X hiding ((.), id)

import Control.Applicative as X
       (liftA, liftA2)
import Control.Category as X
       (Category, id, (.), (<<<), (>>>))

import Control.Monad as X
       (when, unless, (>=>), (<=<), forM, forM_, mzero)
import Control.Monad.IO.Class as X
       (MonadIO, liftIO)
import Control.Monad.Reader.Class as X
       (MonadReader, ask)
import Control.Error.Util as X
       (failWith, failWithM, (??))

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
