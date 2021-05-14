module Halogen.GraphQL.Hooks.Mutation (useMutation) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either, either)
import Effect.Aff.Class (class MonadAff, liftAff)
import GraphQL.Client.SafeQueryName (safeQueryName)
import GraphQL.Client.ToGqlString (toGqlQueryString)
import GraphQL.Client.Types (class GqlQuery, class QueryClient, Client(..), clientMutation, defMutationOpts)
import Halogen.GraphQL.Error (GqlFailure(..))
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.Hooks (HookM)
import Network.RemoteData (RemoteData(..))

useMutation ::
  forall client qOpts opts query m res qSchema mSchema sSchema.
  QueryClient client qOpts opts =>
  MonadAff m =>
  GqlQuery mSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  HookM m (GqlRemote res)
useMutation decoder optsF queryNameUnsafe query (Client client) = do
  json <- liftAff $ clientMutation opts client queryName $ toGqlQueryString query
  pure $ either (Failure <<< DecodeError) pure (decoder json)

  where 
  opts = optsF (defMutationOpts client)

  queryName = safeQueryName queryNameUnsafe
