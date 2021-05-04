module Halogen.GraphQL.HOC.Query (queryConnect, queryConnect_, queryConnectFullRes, queryConnectFullRes_, watchQueryEmitter) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Effect.Aff.Class (class MonadAff)
import GraphQL.Client.Query (decodeGqlRes, getFullRes)
import GraphQL.Client.SafeQueryName (safeQueryName)
import GraphQL.Client.ToGqlString (class GqlQueryString, toGqlQueryString)
import GraphQL.Client.Types (class GqlQuery, class QueryClient, class WatchQueryClient, Client(..), GqlRes, watchQueryEventOpts)
import Halogen as H
import Halogen.GraphQL.Error (GqlFailure(..))
import Halogen.HTML as HH
import Halogen.Subscription (Emitter)
import Network.RemoteData (RemoteData(..))
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

type WithGql res r
  = ( gql :: RemoteData GqlFailure res | r )

data Action input output res
  = Initialize
  | Finalize
  | QueryUpdate (Either JsonDecodeError res)
  | Receive input
  | Emit output

_inner = Proxy :: Proxy "inner"

-- | Pass the result of a graphQL query to a component using the "gql" label
queryConnect ::
  forall baseClient querySchema mutationSchema subscriptionSchema query input output m res gqlQuery opts mOpts.
  MonadAff m =>
  GqlQuery querySchema gqlQuery res =>
  Row.Lacks "gql" input =>
  QueryClient baseClient opts mOpts =>
  WatchQueryClient baseClient opts =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client baseClient querySchema mutationSchema subscriptionSchema ->
  H.Component query { | (WithGql res input) } output m ->
  H.Component query { | input } output m
queryConnect decoder = 
  queryConnectInternal (Proxy :: _ "gql") (decodeGqlRes decoder)

-- | Pass the result of a graphQL query to a component using a custom label
queryConnect_ ::
  forall baseClient querySchema mutationSchema subscriptionSchema query input output m res gqlQuery withGql sym opts mOpts.
  MonadAff m =>
  GqlQuery querySchema gqlQuery res =>
  Row.Lacks sym input =>
  IsSymbol sym =>
  Row.Cons sym (RemoteData GqlFailure res) input withGql =>
    QueryClient baseClient opts mOpts =>
  WatchQueryClient baseClient opts =>
  Proxy sym ->
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client baseClient querySchema mutationSchema subscriptionSchema ->
  H.Component query { | withGql } output m ->
  H.Component query { | input } output m
queryConnect_ sym decoder = 
  queryConnectInternal sym (decodeGqlRes decoder)

-- | Pass the full graphQL result of a graphQL query to a component using the "gql" label
queryConnectFullRes ::
  forall baseClient querySchema mutationSchema subscriptionSchema query input output m res gqlQuery opts mOpts.
  MonadAff m =>
  GqlQuery querySchema gqlQuery res =>
  Row.Lacks "gql" input =>
    QueryClient baseClient opts mOpts =>
  WatchQueryClient baseClient opts =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client baseClient querySchema mutationSchema subscriptionSchema ->
  H.Component query { | (WithGql (GqlRes res) input) } output m ->
  H.Component query { | input } output m
queryConnectFullRes decoder = 
  queryConnectInternal (Proxy :: _ "gql") (getFullRes decoder)

-- | Pass the full graphQL result of a graphQL query to a component using a custom label
queryConnectFullRes_ ::
  forall baseClient querySchema mutationSchema subscriptionSchema query input output m res gqlQuery withGql sym opts mOpts.
  MonadAff m =>
  GqlQuery querySchema gqlQuery res =>
  Row.Lacks sym input =>
  IsSymbol sym =>
    QueryClient baseClient opts mOpts =>
  WatchQueryClient baseClient opts =>
  Row.Cons sym (RemoteData GqlFailure (GqlRes res)) input withGql =>
  Proxy sym ->
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client baseClient querySchema mutationSchema subscriptionSchema ->
  H.Component query { | withGql } output m ->
  H.Component query { | input } output m
queryConnectFullRes_ sym decoder = 
  queryConnectInternal sym (getFullRes decoder)

queryConnectInternal ::
  forall baseClient querySchema mutationSchema subscriptionSchema query input output m res res_ gqlQuery withGql sym opts mOpts.
  MonadAff m =>
  GqlQuery querySchema gqlQuery res_ =>
  Row.Lacks sym input =>
  IsSymbol sym =>
  Row.Cons sym (RemoteData GqlFailure res) input withGql =>
  QueryClient baseClient opts mOpts =>
  WatchQueryClient baseClient opts =>
  Proxy sym ->
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  ({ | input } -> m gqlQuery) ->
  Client baseClient querySchema mutationSchema subscriptionSchema ->
  H.Component query { | withGql } output m ->
  H.Component query { | input } output m
queryConnectInternal sym decoder optsF queryName query client innerComponent =
  H.mkComponent
    { initialState:
        \pass ->
          { pass: Record.insert sym Loading pass
          , subId: Nothing
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
      let sub = watchQueryEmitter decoder optsF queryName q client
      subId <- H.subscribe $ map QueryUpdate sub
      H.modify_ _ { subId = Just subId }
    QueryUpdate (Right res) -> H.modify_ \st -> st { pass = Record.set sym (Success res) st.pass }
    QueryUpdate (Left err) -> H.modify_ \st -> st { pass = Record.set sym (Failure $ DecodeError err) st.pass }
    Receive input -> do
      { pass } <- H.get
      let
        gqlProp = Record.get sym pass
      H.modify_ _ { pass = Record.insert sym gqlProp input }
    Emit output -> H.raise output
    Finalize -> do
      { subId } <- H.get
      traverse_ H.unsubscribe subId

  handleQuery :: forall a. query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = H.query _inner unit

  render state = HH.slot _inner unit innerComponent state.pass Emit

watchQueryEmitter ::
  forall query baseClient opts res ss ms querySchema.
  GqlQueryString query =>
  WatchQueryClient baseClient opts =>
  (Json -> Either JsonDecodeError res) ->
  (opts -> opts) ->
  String ->
  query ->
  Client baseClient querySchema ms ss ->
  Emitter (Either JsonDecodeError res)
watchQueryEmitter decoder optsF queryNameUnsafe q (Client client) = do
  decodeGqlRes decoder <$> watchQueryEventOpts optsF client query
  where
  queryName = safeQueryName queryNameUnsafe

  query = "query " <> queryName <> " " <> toGqlQueryString q