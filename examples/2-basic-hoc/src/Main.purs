module Main where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import GraphQL.Client.Args (type (==>), (=>>))
import GraphQL.Client.BaseClients.Apollo (ApolloClient, createClient)
import GraphQL.Client.Types (class GqlQuery, Client)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.GraphQL.Error (GqlFailure)
import Halogen.GraphQL.HOC.Query (queryConnect)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData)
import Prim.Row as Row
import Type.Proxy (Proxy(..))

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    client <-
      liftEffect
        $ createClient
            { authToken: Nothing
            , headers: []
            , url: "http://localhost:4000/graphql"
            }

    runUI (component client) {} body


component ::
  forall q o m.
  MonadAff m =>
  Client ApolloClient Schema Void Void ->
  H.Component q {} o m
component client =
  myQueryConnect "get_widgets" gqlQuery client
    $ H.mkComponent
        { initialState: { input: _ }
        , render
        , eval: H.mkEval $ H.defaultEval 
            { handleAction = handleAction 
            , receive = Just <<< Receive
            }
        }
  where
  gqlQuery _ = pure { widgets: { id: 1 } =>> { name } }

  render :: State -> _
  render state =
    HH.div_
      [ HH.h2_ [HH.text "Query result:"]
      , HH.pre_ [ HH.text $ show state.input.gql ]
      ]

  handleAction :: Action -> H.HalogenM State Action _ _ _ Unit
  handleAction = case _ of
    Receive input -> H.modify_ _ { input = input }

-- make queries 

myQueryConnect ::
  forall query res m o st q.
  MonadAff m =>
  GqlQuery Schema query res =>
  Row.Lacks "gql" st =>
  DecodeJson res =>
  String ->
  (Record st -> m query) ->
  Client ApolloClient Schema Void Void ->
  H.Component q
    { gql :: RemoteData GqlFailure res
    | st
    }
    o
    m ->
  H.Component q { | st } o m
myQueryConnect = queryConnect decodeJson identity

-- Component types
type State
  = { input ::
        { gql ::
            RemoteData GqlFailure
              { widgets ::
                  Array
                    { name :: String
                    }
              }
        }
    }

data Action
  = Receive
    { gql ::
        RemoteData GqlFailure
          { widgets ::
              Array
                { name :: String
                }
          }
    }

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
