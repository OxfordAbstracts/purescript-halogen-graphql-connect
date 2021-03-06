module Main where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import GraphQL.Client.Args (type (==>))
import GraphQL.Client.BaseClients.Apollo (ApolloClient, createClient)
import GraphQL.Client.Types (class GqlQuery, Client)
import Halogen (Component, liftEffect)
import Halogen.Aff as HA
import Halogen.GraphQL.Error (GqlFailure)
import Halogen.GraphQL.Hooks.Query (GqlQueryHook)
import Halogen.GraphQL.Hooks.Query as GqlHook
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    client :: GqlClient <-
      liftEffect
        $ createClient
            { authToken: Nothing
            , headers: []
            , url: "http://localhost:4000/graphql"
            }
    runUI app { client } body

app :: forall q o. Component q { client :: GqlClient } o Aff
app =
  Hooks.component \_ { client } -> Hooks.do
    res <-
      client
        # query "get_widgets"
            { widgets: { name }
            }
    Hooks.pure do
      HH.div_
        [ HH.div_ [ HH.text "Query result:" ]
        , case res of
            Success { widgets } ->
              HH.div_
                $ widgets
                <#> \widget ->
                    HH.div_ [ HH.text widget.name ]
            Failure err -> HH.text $ show err
            _ -> HH.text "Loading"
        ]

query ::
  forall m query res.
  MonadAff m =>
  DecodeJson res =>
  GqlQuery Schema query res =>
  String -> query -> GqlClient -> GqlQueryHook m (RemoteData GqlFailure res)
query = GqlHook.useQuery decodeJson identity

-- Client type
type GqlClient
  = Client ApolloClient Schema Void Void

-- Schema
type Schema
  = { prop :: String
    , widgets :: { id :: Int } ==> Array Widget
    }

type Widget
  = { name :: String
    , id :: Int
    }

-- Symbols 
prop :: Proxy "prop"
prop = Proxy

name :: Proxy "name"
name = Proxy
