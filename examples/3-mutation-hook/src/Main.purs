module Main where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Array ((..))
import Data.Either (fromRight)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Generated.Enum.Colour (Colour(..))
import Generated.Schema.Gql (Query, Mutation)
import Generated.Symbols (colour, id, name)
import GraphQL.Client.Args (onlyArgs)
import GraphQL.Client.BaseClients.Apollo (ApolloClient, createClient, updateCacheJson)
import GraphQL.Client.BaseClients.Apollo.FetchPolicy (FetchPolicy(..))
import GraphQL.Client.Types (class GqlQuery, Client)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.GraphQL.Hooks.Mutation (useMutation)
import Halogen.GraphQL.Hooks.Query (GqlQueryHook, useQuery)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
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
  where
  app :: forall q o. H.Component q {client :: GqlClient} o Aff
  app =
    Hooks.component \_ {client} -> Hooks.do
      Hooks.pure do
        HH.div
          [ HP.style "margin:1rem" ]
          [ HH.slot_ (Proxy :: Proxy "viewMutation") unit viewMutation {client}
          , HH.slot_ (Proxy :: Proxy "viewQuery") unit viewQuery {client}
          ]

  viewQuery :: forall q o. H.Component q {client :: GqlClient} o Aff
  viewQuery =
    Hooks.component \_ {client} -> Hooks.do
      res <- client # query "get_widgets" getWidgets
      Hooks.pure do
        HH.div
          [ HP.style "margin-top:2rem" ]
          [ HH.div_ [ HH.text "Query result:" ]
          , case res of
              Success { widgets } -> HH.div_ $ map viewWidget widgets
              Failure err -> HH.text $ show err
              _ -> HH.text "Loading"
          ]
    where
    viewWidget { id, name, colour } =
      HH.div
        [ HP.style $ "color:" <> show colour ]
        [ HH.text $ show id <> ". " <> name ]

  getWidgets = { widgets: { name, colour, id } }

  viewMutation :: forall q o. H.Component q {client :: GqlClient} o Aff
  viewMutation =
    Hooks.component \_ {client} -> Hooks.do
      widgetId /\ setWidgetId <- Hooks.useState Nothing
      colour /\ setColour <- Hooks.useState GREEN
      let
        handleIdChange str = Hooks.put setWidgetId $ Int.fromString str

        handleColourChange str = Hooks.put setColour $ fromRight colour $ decodeJson $ encodeJson str

        mutateColour _ev = case widgetId of
          Nothing -> pure unit
          Just id -> do
            void $ client
              # mutation "set_colour"
                  { set_widget_colour: onlyArgs { colour, id }
                  }
            liftEffect
              $ updateCacheJson client getWidgets \res ->
                  res
                    { widgets =
                      res.widgets
                        <#> \w ->
                            if w.id == id then w { colour = colour } else w
                    }
      Hooks.pure do
        HH.div
          []
          [ HH.div_ [ HH.text "Set widget colour:" ]
          , HH.select
              [ HE.onValueChange handleIdChange
              ]
              $ [ HH.option
                    [ HP.selected $ widgetId == Nothing ]
                    []
                ]
              <> ( 1 .. 3
                    <#> \i ->
                        HH.option
                          [ HP.selected $ Just i == widgetId
                          , HP.value $ show i
                          ]
                          [ HH.text $ show i ]
                )
          , HH.select
              [ HE.onValueChange handleColourChange ]
              $ [ colourOption RED
                , colourOption BLUE
                , colourOption GREEN
                ]
          , HH.button
              [ HP.style $ guard (isJust widgetId) "cursor:pointer"
              , HP.disabled $ isNothing widgetId
              , HE.onClick mutateColour
              ]
              [ HH.text "Set colour" ]
          ]
    where
    colourOption colour =
      HH.option
        [ HP.selected $ colour == colour
        , HP.value $ show colour
        ]
        [ HH.text $ show colour ]

query ::
  forall m query res.
  MonadAff m =>
  DecodeJson res =>
  GqlQuery Query query res =>
  String -> query -> GqlClient -> GqlQueryHook m res
query = useQuery decodeJson _ { fetchPolicy = Just NetworkOnly }

mutation ::
  forall m query res.
  MonadAff m =>
  DecodeJson res =>
  GqlQuery Mutation query res =>
  String -> query -> GqlClient -> HookM m (GqlRemote res)
mutation = useMutation decodeJson identity

-- Client type
type GqlClient
  = Client ApolloClient Query Mutation Void
