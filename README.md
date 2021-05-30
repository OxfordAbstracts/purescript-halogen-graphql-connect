# purescript-halogen-graphql-connect

Make GraphQL calls in your halogen project, using hooks or higher order components. 

This library will allow you to make typesafe Graphql queries, mutations and subscriptions in your Halogen App, 
via [Apollo.js](www.apollographql.com) 

This library relies on a lower level purescript Graphql Client that can be used without Halogen,
 [purescript-graphql-client](https://github.com/OxfordAbstracts/purescript-graphql-client)

## Example 

This is an example of a basic Halogen app that makes a query using hooks and displays the result:

```purs
module Main where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import GraphQL.Client.Args (type (==>), (=>>))
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
    runUI app {client} body

app :: forall q o. Component q {client :: GqlClient} o Aff
app  =
  Hooks.component \_ {client} -> Hooks.do
    res <-
      client
        # query "get_widgets"
            { widgets: { name }
            }
    Hooks.pure do
      HH.div_
        [ HH.div_ [ HH.text "Query result:" ]
        , case res of
            Success { widgets } -> HH.p_ $ map (HH.text <<< _.name) widgets
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
```

## Table of contents

- [purescript-halogen-graphql-connect](#purescript-halogen-graphql-connect)
  - [Example](#example)
  - [Table of contents](#table-of-contents)
  - [Graphql Hooks](#graphql-hooks)
  - [Higher order componenets (HOC)](#higher-order-componenets-hoc)
  - [Syntax](#syntax)

To see this example in full look at the `examples/1-basic-hook` directory

## Graphql Hooks 

This is probably the easiest way to use this library. You can use hooks to make graphql 
queries, mutations and subscriptions. 

For examples on this look at the examples directories that end with "-hook"

## Higher order componenets (HOC)

Use higher order components if you prefer not to use hooks. These can be used to make queries or 
subscriptions. For mutations, you can use the [purescript-graphql-client](www.github.com/OxfordAbstracts/puresrcipt-graphql-client) directly. 

For examples on this look at the examples directories that end with "-hoc"


## Syntax

For query syntax look at [purescript-graphql-client](www.github.com/OxfordAbstracts/puresrcipt-graphql-client) 
