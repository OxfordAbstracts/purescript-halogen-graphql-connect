module Halogen.GraphQL.Hooks.Query (GqlQueryHook, UseQuery, useQuery, useQueryM) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import GraphQL.Client.SafeQueryName (safeQueryName)
import GraphQL.Client.ToGqlString (toGqlQueryString)
import GraphQL.Client.Types (class GqlQuery, class QueryClient, class WatchQueryClient, Client(..), clientQuery, defQueryOpts)
import Halogen.GraphQL.Error (GqlFailure(..))
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.GraphQL.HOC.Query (watchQueryEmitter)
import Halogen.Hooks (type (<>), Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..))

type UseQuery res
  = UseState (GqlRemote res) <> UseEffect <> Hooks.Pure

type GqlQueryHook m res = Hook m (UseQuery res) (GqlRemote res)

-- | Run a graphql query on component initialization, re-emitting when local cache updates
useQuery ::
  forall client query m res qSchema mSchema sSchema opts.
  WatchQueryClient client opts =>
  GqlQuery qSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  GqlQueryHook m res
useQuery decoder opts queryName query client = Hooks.do
  result /\ resultId <- Hooks.useState NotAsked
  Hooks.useLifecycleEffect do
    Hooks.put resultId Loading
    let
      emitter = watchQueryEmitter decoder opts queryName query client

      handler res =
        Hooks.put resultId
          $ either (Failure <<< DecodeError) pure res
    hookSubId <- Hooks.subscribe $ map handler emitter
    pure $ Just $ Hooks.unsubscribe hookSubId
  Hooks.pure result


-- | Run a graphql query in an event handler, in the `HookM` monad
useQueryM ::
  forall client query m res mOpts qSchema mSchema sSchema opts.
  QueryClient client opts mOpts =>
  MonadAff m =>
  GqlQuery qSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  HookM m (GqlRemote res)
useQueryM decoder optsF queryNameUnsafe query (Client client) = do
  json <- liftAff $ clientQuery opts client queryName $ toGqlQueryString query
  pure $ either (Failure <<< DecodeError) pure (decoder json)

  where 
  opts = optsF (defQueryOpts client)

  queryName = safeQueryName queryNameUnsafe
