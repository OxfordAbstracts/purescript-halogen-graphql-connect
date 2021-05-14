module Halogen.GraphQL.Hooks.Subscription (UseSubscription, useSubscription) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import GraphQL.Client.Types (class GqlQuery, class SubscriptionClient, Client)
import Halogen.GraphQL.Error (GqlFailure(..))
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.GraphQL.HOC.Subscription (subscriptionEmitter)
import Halogen.Hooks (type (<>), Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..))

type UseSubscription res
  = UseState (GqlRemote res) <> UseEffect <> Hooks.Pure

type GqlSubscriptionHook m res = Hook m (UseSubscription res) (GqlRemote res)

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
useSubscription decoder opts queryName query client = Hooks.do
  result /\ resultId <- Hooks.useState NotAsked
  Hooks.useLifecycleEffect do
    Hooks.put resultId Loading
    let
      emitter = subscriptionEmitter decoder opts queryName query client

      handler res =
        Hooks.put resultId
          $ either (Failure <<< DecodeError) pure res
    hookSubId <- Hooks.subscribe $ map handler emitter
    pure $ Just $ Hooks.unsubscribe hookSubId
  Hooks.pure result


type UseSubscriptionFold res
  = UseState res <> UseEffect <> Hooks.Pure

type GqlSubscriptionAppendHook m res = Hook m (UseSubscriptionFold res) (GqlRemote res)

-- | Listen to a graphql subscription on component initialization and append new results to an array
useSubscriptionFold ::
  forall client query m res qSchema mSchema sSchema opts.
  SubscriptionClient client opts =>
  GqlQuery sSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  _ ->
  String ->
  query ->
  Client client qSchema mSchema sSchema ->
  GqlSubscriptionHook m res
useSubscriptionFold decoder opts errorHandler queryName query client = Hooks.do
  result /\ resultId <- Hooks.useState NotAsked
  Hooks.useLifecycleEffect do
    Hooks.put resultId Loading
    let
      emitter = subscriptionEmitter decoder opts queryName query client

      handler res =
        case res of 
          Right -> 
        Hooks.mo resultId
          $ either (Failure <<< DecodeError) pure res
    hookSubId <- Hooks.subscribe $ map handler emitter
    pure $ Just $ Hooks.unsubscribe hookSubId
  Hooks.pure result