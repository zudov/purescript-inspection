module Package where

import Prelude

import Control.Monad.Aff (runAff, Aff())

import Data.Array ((:))
import Data.Array as Array
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Foldable
import Data.Function
import Data.Either
import Data.Generic
import Data.Maybe
import Data.Tuple
import Data.List (List())
import Data.List as List
import Data.String as String
import Data.StrMap (StrMap())
import Data.StrMap as StrMap

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax as Affjax

data Query a
  = UpdatePackageMatrix a

data BuildResult
  = Success
  | Failure

instance decodeJsonBuildResult :: DecodeJson BuildResult where
  decodeJson j = case toString j of
                   Nothing -> Left "Expected a string"
                   Just "Success" -> Right Success
                   Just "Failure" -> Right Failure

instance showBuildResult :: Show BuildResult where
  show Success = "Success"
  show Failure = "Failure"

instance eqBuildResult :: Eq BuildResult where
  eq = gEq

instance ordBuildResult :: Ord BuildResult where
  compare = gCompare

derive instance genericBuildResult :: Generic BuildResult

type PackageVersion = String
type Compiler = String

--  Map PackageVersion (Map Compiler [BuildResult])
newtype PackageMatrix = PackageMatrix (StrMap (StrMap (Array BuildResult)))

instance decodeJsonPackageMatrix :: DecodeJson PackageMatrix where
  decodeJson = map PackageMatrix <<< decodeJson

compilers :: PackageMatrix -> Array Compiler
compilers (PackageMatrix matrix) = Array.nub $ foldMap StrMap.keys matrix

versions :: PackageMatrix -> Array Compiler
versions (PackageMatrix matrix) = StrMap.keys matrix

packageTable :: PackageMatrix -> { rowNames :: Array PackageVersion
                                 , colNames :: Array Compiler
                                 , content :: Array (Array (Maybe (Array BuildResult)))
                                 }
packageTable m@(PackageMatrix matrix) = { rowNames, colNames, content }
  where
    rowNames = Array.sort $ versions m
    colNames = Array.sort $ compilers m
    content  = rowNames <#>
                  \rowName -> colNames <#>
                    \colName -> StrMap.lookup rowName matrix
                            >>= StrMap.lookup colName

data State = State PackageMatrix

type Effects eff = HalogenEffects (ajax :: AJAX | eff)

packageMatrix :: forall eff. Component State Query (Aff (Effects eff))
packageMatrix = component render eval
  where
    render :: State -> ComponentHTML Query
    render (State matrix@(PackageMatrix m)) =
      H.table_ 
        [ H.thead_
            [ H.tr_ (H.th_ [] : (H.th_ <<< Array.singleton
                                       <<< H.text
                                       <<< (\n -> fromMaybe n (String.stripPrefix "purescript-" n))
                                       <$> colNames)) ]
        , H.tbody_
            (rowNames <#>
               \rowName -> H.tr_ (H.td_ [ H.text rowName ] : (colNames <#>
                 \colName ->
                    case StrMap.lookup rowName m >>= StrMap.lookup colName of
                      Nothing      -> H.td [ P.class_ (H.className "missing") ] []
                      Just builds  -> case Array.uncons builds of
                        Nothing ->
                          H.td_  []
                        Just { head = Success } ->
                          H.td [ P.class_ (H.className "success") ]
                               [ H.text "+" ]
                        Just { head = Failure } ->
                          H.td [ P.class_ (H.className "failure") ]
                               [ H.text "-" ]
                    )))
                            
          
        ]
      where
        rowNames = Array.sort $ versions matrix
        colNames = Array.sort $ compilers matrix

    eval :: Natural Query (ComponentDSL State Query (Aff (Effects eff)))
    eval (UpdatePackageMatrix a) = pure a
