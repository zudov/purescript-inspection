module Main where

import Prelude

import Control.Bind
import Control.Monad.Aff (runAff, Aff(), later')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe
import Control.Monad.Eff.Exception (throwException)
import Data.Functor
import Data.Functor.Coproduct (Coproduct(), left, right)
import Data.Array as Array
import Data.Array.Unsafe (head)
import Data.Generic
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Either
import Data.Either.Unsafe
import Data.Monoid
import Data.Tuple
import Data.StrMap (StrMap())
import Data.StrMap as StrMap
import Data.Foreign
import Data.Argonaut.Decode
import Data.Argonaut.Parser

import Halogen
import Halogen.Util (appendToBody)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Events.Forms as E

import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Response (Respondable, ResponseType(..))

import Package as Package

main = runAff throwException (const (pure unit)) $ do
  x <- Affjax.get "http://localhost:8080/matrix"
  liftEff $ logAny x
  liftEff $ logAny $ jsonParser x.response
  let ePackages = decodeJson =<< jsonParser x.response
  liftEff $ logAny (ePackages :: _ Packages)
  let packages = fromRight ePackages
  app <- runUI ui (installedState { packages: packages
                                  , selectedPackage: head $ packageNames packages
                                  })
  appendToBody app.node

type PackageName = String

newtype Packages = Packages (StrMap Package.PackageMatrix)

lookupPackageMatrix :: PackageName -> Packages -> Maybe Package.PackageMatrix
lookupPackageMatrix n (Packages m) = StrMap.lookup n m

packageNames :: Packages -> Array PackageName
packageNames (Packages m) = StrMap.keys m

instance decodeJsonPackages :: DecodeJson Packages where
  decodeJson = map Packages <<< decodeJson

newtype PackageSlot = PackageSlot PackageName

instance ordPackageSlot :: Ord PackageSlot where
  compare (PackageSlot a) (PackageSlot b) = compare a b

instance eqPackageSlot :: Eq PackageSlot where
  eq (PackageSlot a) (PackageSlot b) = eq a b

type State = { packages :: Packages
             , selectedPackage :: PackageName
             }

data Query a
  = SelectPackage PackageName a 
  | UpdatePackages Packages a

type StateP g = InstalledState State Package.State Query Package.Query g PackageSlot
type QueryP = Coproduct Query (ChildF PackageSlot Package.Query)

type Effects eff = HalogenEffects (ajax :: AJAX | eff)

ui :: forall eff. Component (StateP (Aff (Effects eff))) QueryP (Aff (Effects eff))
ui = parentComponent render eval
  where
    render :: State -> ParentHTML Package.State Query Package.Query (Aff (Effects eff)) PackageSlot
    render state =
      H.div_
        [ H.select
        [ E.onValueChange (E.input SelectPackage) ]
            (H.option_ <<< Array.singleton <<< H.text <$> packageNames state.packages)
        , H.slot (PackageSlot state.selectedPackage)
                 (\_ -> { component: Package.packageMatrix
                        , initialState:
                            Package.State $ fromJust
                                          $ lookupPackageMatrix state.selectedPackage
                                                                state.packages
                        })
        ]
   
    eval :: Natural Query (ParentDSL State Package.State Query Package.Query (Aff (Effects eff)) PackageSlot)
    eval (SelectPackage name next) = modify (_ { selectedPackage = name }) $> next
    eval (UpdatePackages packages next) = modify (_ { packages = packages }) $> next

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
