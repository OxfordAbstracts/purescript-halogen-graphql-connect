module Halogen.GraphQL.Subscription (subConnect, subConnect_, subConnectFullRes, subConnectFullRes_) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff.Class (class MonadAff)
import GraphQL.Client.BaseClients.Apollo (ApolloSubClient, QueryOpts)
import GraphQL.Client.Query (decodeGqlRes, getFullRes)
import GraphQL.Client.SafeQueryName (safeQueryName)
import GraphQL.Client.ToGqlString (class GqlQueryString, toGqlQueryString)
import GraphQL.Client.Types (class GqlQuery, class QueryClient, class SubscriptionClient, Client(..), GqlRes, clientSubscription, defSubOpts)
import Halogen as H
import Halogen.GraphQL.Error (GqlFailure(..))
import Halogen.HTML as HH
import Halogen.Query.EventSource (EventSource, Finalizer(..), effectEventSource, emit)
import Network.RemoteData (RemoteData(..))
import Prim.Row as Row
import Record as Record

type WithGql res r
  = ( subGql :: RemoteData GqlFailure res | r )

data Action input output res
  = Initialize
  | Finalize
  | QueryUpdate (Either JsonDecodeError res)
  | Receive input
  | Emit output

_inner = SProxy :: SProxy "inner"

-- | Pass the result of a graphQL query to a component using the "subGql" label
subConnect ::
  forall querySchema mutationSchema subscriptionSchema query input output m res gqlQuery.
  MonadAff m =>
  GqlQuery subscriptionSchema gqlQuery res =>
  Row.Lacks "subGql" input =>
  (Json -> Either JsonDecodeError res) ->
  (QueryOpts -> QueryOpts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client ApolloSubClient querySchema mutationSchema subscriptionSchema ->
  H.Component HH.HTML query { | (WithGql res input) } output m ->
  H.Component HH.HTML query { | input } output m
subConnect decoder = 
  subConnectInternal (SProxy :: _ "subGql") (decodeGqlRes decoder)

-- | Pass the result of a graphQL query to a component using a custom label
subConnect_ ::
  forall querySchema mutationSchema subscriptionSchema query input output m res gqlQuery withGql sym.
  MonadAff m =>
  GqlQuery subscriptionSchema gqlQuery res =>
  Row.Lacks sym input =>
  IsSymbol sym =>
  Row.Cons sym (RemoteData GqlFailure res) input withGql =>
  SProxy sym ->
  (Json -> Either JsonDecodeError res) ->
  (QueryOpts -> QueryOpts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client ApolloSubClient querySchema mutationSchema subscriptionSchema ->
  H.Component HH.HTML query { | withGql } output m ->
  H.Component HH.HTML query { | input } output m
subConnect_ sym decoder = 
  subConnectInternal sym (decodeGqlRes decoder)

-- | Pass the full graphQL result of a graphQL query to a component using the "subGql" label
subConnectFullRes ::
  forall querySchema mutationSchema subscriptionSchema query input output m res gqlQuery.
  MonadAff m =>
  GqlQuery subscriptionSchema gqlQuery res =>
  Row.Lacks "subGql" input =>
  (Json -> Either JsonDecodeError res) ->
  (QueryOpts -> QueryOpts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client ApolloSubClient querySchema mutationSchema subscriptionSchema ->
  H.Component HH.HTML query { | (WithGql (GqlRes res) input) } output m ->
  H.Component HH.HTML query { | input } output m
subConnectFullRes decoder = 
  subConnectInternal (SProxy :: _ "subGql") (getFullRes decoder)

-- | Pass the full graphQL result of a graphQL query to a component using a custom label
subConnectFullRes_ ::
  forall querySchema mutationSchema subscriptionSchema query input output m res gqlQuery withGql sym.
  MonadAff m =>
  GqlQuery subscriptionSchema gqlQuery res =>
  Row.Lacks sym input =>
  IsSymbol sym =>
  Row.Cons sym (RemoteData GqlFailure (GqlRes res)) input withGql =>
  SProxy sym ->
  (Json -> Either JsonDecodeError res) ->
  (QueryOpts -> QueryOpts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client ApolloSubClient querySchema mutationSchema subscriptionSchema ->
  H.Component HH.HTML query { | withGql } output m ->
  H.Component HH.HTML query { | input } output m
subConnectFullRes_ sym decoder = 
  subConnectInternal sym (getFullRes decoder)

subConnectInternal ::
  forall querySchema mutationSchema subscriptionSchema query input output m res res_ gqlQuery withGql sym.
  MonadAff m =>
  GqlQuery subscriptionSchema gqlQuery res_ =>
  Row.Lacks sym input =>
  IsSymbol sym =>
  Row.Cons sym (RemoteData GqlFailure res) input withGql =>
  SProxy sym ->
  (Json -> Either JsonDecodeError res) ->
  (QueryOpts -> QueryOpts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client ApolloSubClient querySchema mutationSchema subscriptionSchema ->
  H.Component HH.HTML query { | withGql } output m ->
  H.Component HH.HTML query { | input } output m
subConnectInternal sym decoder optsF queryName query client innerComponent =
  H.mkComponent
    { initialState:
        \pass ->
          { pass: Record.insert sym Loading pass
          , subId: Nothing
          , finalized: false
          }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              , finalize = Just Finalize
              , receive = Just <<< Receive
              }
    }
  where
  handleAction = case _ of
    Initialize -> do
      { pass } <- H.get
      q <- H.lift $ query $ Record.delete sym pass
      sub <- H.lift $ subscriptionEventSource decoder optsF queryName q client
      subId <- H.subscribe $ map QueryUpdate sub
      H.modify_ _ { subId = Just subId }
    QueryUpdate (Right res) -> H.modify_ \st -> st { pass = Record.set sym (Success res) st.pass }
    QueryUpdate (Left err) -> H.modify_ \st -> st { pass = Record.set sym (Failure $ DecodeError err) st.pass }
    Receive input -> do
      { pass } <- H.get
      let
        subGql = Record.get sym pass
      H.modify_ _ { pass = Record.insert sym subGql input }
    Emit output -> H.raise output
    Finalize -> do
      { subId } <- H.modify _ { finalized = true }
      traverse_ H.unsubscribe subId

  handleQuery :: forall a. query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = H.query _inner unit

  render state =
    if state.finalized then 
      HH.text "" 
    else
      HH.slot _inner unit innerComponent state.pass (Just <<< Emit)

subscriptionEventSource ::
  forall query m baseClient opts mOpts res ss ms querySchema.
  GqlQueryString query =>
  Applicative m =>
  MonadAff m =>
  SubscriptionClient baseClient opts =>
  QueryClient baseClient opts mOpts =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client baseClient querySchema ms ss ->
  m (EventSource m (Either JsonDecodeError res))
subscriptionEventSource decoder optsF queryNameUnsafe q (Client client) = do
  pure
    $ effectEventSource \emitter -> do
        cancel <-
          clientSubscription
            (optsF (defSubOpts client))
            client
            query
            (decoder >>> emit emitter)
        pure $ Finalizer cancel
  where
  queryName = safeQueryName queryNameUnsafe

  query = "subscription " <> queryName <> " " <> toGqlQueryString q
