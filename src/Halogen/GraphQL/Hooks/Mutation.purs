module Halogen.GraphQL.Hooks.Query (UseMutation, useMutation) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import GraphQL.Client.BaseClients.Apollo (ApolloClient, MutationOpts)
import GraphQL.Client.SafeQueryName (safeQueryName)
import GraphQL.Client.ToGqlString (toGqlQueryString)
import GraphQL.Client.Types (class GqlQuery, Client(..), clientMutation, defMutationOpts)
import Halogen.GraphQL.Error (GqlFailure(..))
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.Hooks (type (<>), Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..))

type UseMutation res
  = UseState (GqlRemote res) <> UseEffect <> Hooks.Pure

useMutation ::
  forall query m res qSchema mSchema sSchema.
  MonadAff m =>
  GqlQuery mSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (MutationOpts -> MutationOpts) ->
  String ->
  query ->
  Client ApolloClient qSchema mSchema sSchema ->
  Hook m (UseMutation res) (GqlRemote res)
useMutation decoder optsF queryNameUnsafe query (Client client) = Hooks.do
  result /\ resultId <- Hooks.useState Loading

  Hooks.useLifecycleEffect do
    json <- liftAff $ clientMutation opts client queryName $ toGqlQueryString query
    Hooks.put resultId $ either (Failure <<< DecodeError) pure (decoder json)
    pure Nothing

  Hooks.pure result

  where 
  opts = optsF (defMutationOpts client)

  queryName = safeQueryName queryNameUnsafe
