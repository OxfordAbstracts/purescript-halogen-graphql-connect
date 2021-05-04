module Halogen.GraphQL.Hooks.Query (UseQuery, GqlRemote, useQuery) where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import GraphQL.Client.BaseClients.Apollo (ApolloClient, QueryOpts)
import GraphQL.Client.Types (class GqlQuery, Client)
import Halogen.GraphQL.Error (GqlFailure(..))
import Halogen.GraphQL.HOC.Query (watchQueryEmitter)
import Halogen.Hooks (type (<>), Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Network.RemoteData (RemoteData(..))

type UseQuery res
  = UseState (GqlRemote res) <> UseEffect <> Hooks.Pure

type GqlRemote res
  = RemoteData GqlFailure res

useQuery ::
  forall query m res qSchema mSchema sSchema.
  MonadAff m =>
  GqlQuery qSchema query res =>
  (Json -> Either JsonDecodeError res) ->
  (QueryOpts -> QueryOpts) ->
  String ->
  query ->
  Client ApolloClient qSchema mSchema sSchema ->
  Hook m (UseQuery res) (GqlRemote res)
useQuery decoder opts queryName query client = Hooks.do
  result /\ resultId <- Hooks.useState Loading
  Hooks.useLifecycleEffect do
    let
      emitter = watchQueryEmitter decoder opts queryName query client
    subscription <-
      liftEffect $ HS.subscribe emitter
        $ \res ->
            pure $ Hooks.put resultId $ either (Failure <<< DecodeError) pure res
    lift $ pure $ Just $ liftEffect $ HS.unsubscribe subscription
  Hooks.pure result
