module Halogen.GraphQL.Hooks.Query
  ( GqlQueryHook
  , UseQuery
  , useQuery
  , useQueryFullRes
  , useQueryM
  , useQueryMFullRes
  ) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either, either, hush)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import GraphQL.Client.Query (decodeGqlRes, getFullRes)
import GraphQL.Client.SafeQueryName (safeQueryName)
import GraphQL.Client.ToGqlString (toGqlQueryString)
import GraphQL.Client.Types (class GqlQuery, class QueryClient, class WatchQueryClient, Client(..), GqlRes, clientQuery, defQueryOpts)
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.GraphQL.HOC.Query (watchQueryEmitter)
import Halogen.GraphQL.Internal.Util (checkErrorsAndDecode)
import Halogen.Hooks (type (<>), Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..))

type UseQuery res
  = UseState res <> UseEffect <> Hooks.Pure

type GqlQueryHook m res
  = Hook m (UseQuery res) res

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
  GqlQueryHook m (GqlRemote res)
useQuery decoder opts queryName query client = Hooks.do
  result /\ resultId <- Hooks.useState NotAsked
  Hooks.useLifecycleEffect do
    Hooks.put resultId Loading
    let
      emitter = watchQueryEmitter true (decodeGqlRes decoder) opts queryName query client

      handler res =
        Hooks.put resultId
          $ either Failure pure res
    hookSubId <- Hooks.subscribe $ map handler emitter
    pure $ Just $ Hooks.unsubscribe hookSubId
  Hooks.pure result

-- | Run a graphql query on component initialization, re-emitting when local cache updates.
-- | Returns the full GraphQL response
useQueryFullRes ::
  forall client query m res qSchema mSchema sSchema opts.
  WatchQueryClient client opts =>
  GqlQuery qSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  GqlQueryHook m (Maybe (GqlRes res))
useQueryFullRes decoder opts queryName query client = Hooks.do
  result /\ resultId <- Hooks.useState Nothing
  Hooks.useLifecycleEffect do
    let
      emitter = watchQueryEmitter false ((map pure <<< getFullRes) decoder) opts queryName query client

      handler res =
        Hooks.put resultId $ hush res
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
  pure $ either Failure pure (checkErrorsAndDecode true decoder json)
  where
  opts = optsF (defQueryOpts client)

  queryName = safeQueryName queryNameUnsafe

-- | Run a graphql query in an event handler, in the `HookM` monad
-- | Returns the full GraphQL response
useQueryMFullRes ::
  forall client query m res mOpts qSchema mSchema sSchema opts.
  QueryClient client opts mOpts =>
  MonadAff m =>
  GqlQuery qSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  HookM m (GqlRes res)
useQueryMFullRes decoder optsF queryNameUnsafe query (Client client) = do
  json <- liftAff $ clientQuery opts client queryName $ toGqlQueryString query
  pure $ getFullRes decoder json
  where
  opts = optsF (defQueryOpts client)

  queryName = safeQueryName queryNameUnsafe
