module Halogen.GraphQL.Error where 

import Prelude

import Data.Argonaut (JsonDecodeError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data GqlFailure
  = QueryError (NonEmptyArray String)
  | DecodeError JsonDecodeError

derive instance genericGqlFailure :: Generic GqlFailure _

instance showGqlFailure :: Show GqlFailure where
  show = genericShow

instance eqGqlFailure :: Eq GqlFailure where
  eq = eq `on` show
