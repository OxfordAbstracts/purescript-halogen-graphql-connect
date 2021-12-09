module Halogen.GraphQL.Hooks.Mutation (useMutation, useMutationFullRes) where

import Prelude
import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either, either)
import Effect.Aff.Class (class MonadAff, liftAff)
import GraphQL.Client.Query (getFullRes)
import GraphQL.Client.SafeQueryName (safeQueryName)
import GraphQL.Client.ToGqlString (toGqlQueryString)
import GraphQL.Client.Types (class GqlQuery, class QueryClient, Client(..), GqlRes, clientMutation, defMutationOpts)
import GraphQL.Client.Variables (getVarsJson)
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.GraphQL.Internal.Util (checkErrorsAndDecode)
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
  json <- liftAff $ clientMutation opts client queryName (toGqlQueryString query) (getVarsJson query)
  pure $ either Failure pure (checkErrorsAndDecode true decoder json)
  where
  opts = optsF (defMutationOpts client)

  queryName = safeQueryName queryNameUnsafe

useMutationFullRes ::
  forall client qOpts opts query m res qSchema mSchema sSchema.
  QueryClient client qOpts opts =>
  MonadAff m =>
  GqlQuery mSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  HookM m (GqlRes res)
useMutationFullRes decoder optsF queryNameUnsafe query (Client client) = do
  json <- liftAff $ clientMutation opts client queryName (toGqlQueryString query) (getVarsJson query)
  pure $ getFullRes decoder json
  where
  opts = optsF (defMutationOpts client)

  queryName = safeQueryName queryNameUnsafe
