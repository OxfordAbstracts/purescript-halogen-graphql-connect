module Halogen.GraphQL.Error where 

import Prelude

import Data.Argonaut (JsonDecodeError)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception (Error)

data GqlFailure
  = QueryError Error
  | DecodeError JsonDecodeError

derive instance genericGqlFailure :: Generic GqlFailure _

instance showGqlFailure :: Show GqlFailure where
  show = genericShow

instance eqGqlFailure :: Eq GqlFailure where
  eq = eq `on` show
