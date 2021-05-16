module Halogen.GraphQL.Hooks.Subscription
  ( UseSubscription
  , UseSubscriptionFold
  , GqlSubscriptionHook
  , GqlSubscriptionFoldHook
  , useSubscription
  , useSubscriptionFullRes
  , useSubscriptionAppend
  , useSubscriptionAppendFullRes
  , useSubscriptionFold
  , useSubscriptionFoldFullRes
  ) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import GraphQL.Client.Query (decodeGqlRes, getFullRes)
import GraphQL.Client.Types (class GqlQuery, class SubscriptionClient, Client, GqlRes)
import Halogen.GraphQL.Error (GqlFailure)
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.GraphQL.HOC.Subscription (subscriptionEmitter)
import Halogen.Hooks (type (<>), Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..))

type UseSubscription res
  = UseSubscriptionFold (GqlRemote res)

type GqlSubscriptionHook m res
  = GqlSubscriptionFoldHook m (GqlRemote res)

-- | Listen to a graphql subscription on component initialization
useSubscription ::
  forall client query m res qSchema mSchema sSchema opts.
  SubscriptionClient client opts =>
  GqlQuery sSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  GqlSubscriptionHook m res
useSubscription =
  useSubscriptionFold
    Loading
    (\_ err -> Failure err)
    (\_ res -> Success res)

-- | Listen to a graphql subscription on component initialization
useSubscriptionFullRes ::
  forall client query m res qSchema mSchema sSchema opts.
  SubscriptionClient client opts =>
  GqlQuery sSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  GqlSubscriptionFoldHook m (Maybe (GqlRes res))
useSubscriptionFullRes =
  useSubscriptionFoldFullRes
    Nothing
    (\_ res -> Just res)

type GqlSubscriptionAppendHook m res = GqlSubscriptionFoldHook m (Array (Either GqlFailure res))

-- | Listen to a graphql subscription on component initialization 
-- | and add the new results to an array
useSubscriptionAppend ::
  forall client query qSchema mSchema sSchema opts res m.
  SubscriptionClient client opts =>
  GqlQuery sSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema -> 
  GqlSubscriptionAppendHook m res 
useSubscriptionAppend =
  useSubscriptionFold []
    (\acc err -> acc <> [ Left err ])
    (\acc res -> acc <> [ Right res ])

-- | Listen to a graphql subscription on component initialization 
-- | and add the new results to an array
useSubscriptionAppendFullRes ::
  forall client query qSchema mSchema sSchema opts res m.
  SubscriptionClient client opts =>
  GqlQuery sSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema -> 
   GqlSubscriptionFoldHook m (Array (GqlRes res))
useSubscriptionAppendFullRes =
  useSubscriptionFoldFullRes []
    (\acc res -> acc <> [ res ])

type UseSubscriptionFold res
  = UseState res <> UseEffect <> Hooks.Pure

type GqlSubscriptionFoldHook m res
  = Hook m (UseSubscriptionFold res) res

-- | Listen to a graphql subscription on component initialization 
-- | and specify how to fold the results into an accumulator
useSubscriptionFold ::
  forall client query m res qSchema mSchema sSchema opts acc.
  SubscriptionClient client opts =>
  GqlQuery sSchema query res =>
  acc ->
  (acc -> GqlFailure -> acc) ->
  (acc -> res -> acc) ->
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  GqlSubscriptionFoldHook m acc
useSubscriptionFold = useSubscriptionFoldInternal true decodeGqlRes

-- -- | Listen to a graphql subscription on component initialization 
-- -- | and specify how to fold the results into an accumulator.
-- -- | Gets the full response as per Graphql Spec
useSubscriptionFoldFullRes ::
  forall client query m qSchema mSchema sSchema opts acc res.
  SubscriptionClient client opts =>
  GqlQuery sSchema query res =>
  acc ->
  (acc -> GqlRes res -> acc) ->
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  GqlSubscriptionFoldHook m acc
useSubscriptionFoldFullRes acc foldFn = useSubscriptionFoldInternal false (map Right <<< getFullRes) acc const foldFn

-- | Listen to a graphql subscription on component initialization 
-- | and specify how to fold the results into an accumulator
useSubscriptionFoldInternal ::
  forall client query m dataRes res qSchema mSchema sSchema opts acc.
  SubscriptionClient client opts =>
  GqlQuery sSchema query dataRes =>
  Boolean ->
  ((Json -> Either JsonDecodeError dataRes) -> (Json -> Either JsonDecodeError res)) ->
  acc ->
  (acc -> GqlFailure -> acc) ->
  (acc -> res -> acc) ->
  (Json -> Either JsonDecodeError dataRes) ->
  (opts -> opts) ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  GqlSubscriptionFoldHook m acc
useSubscriptionFoldInternal checkErrors resDecoder init errorHandler foldFn decoder opts queryName query client = Hooks.do
  result /\ resultId <- Hooks.useState init
  Hooks.useLifecycleEffect do
    let
      emitter = subscriptionEmitter checkErrors (resDecoder decoder) opts queryName query client

      handler = case _ of
        Left err -> Hooks.modify_ resultId $ flip errorHandler err
        Right res -> Hooks.modify_ resultId $ flip foldFn res
    hookSubId <- Hooks.subscribe $ map handler emitter
    pure $ Just $ Hooks.unsubscribe hookSubId
  Hooks.pure result
