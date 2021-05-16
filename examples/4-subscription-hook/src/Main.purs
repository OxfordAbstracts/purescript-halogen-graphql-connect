module Main where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, encodeJson, stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign.Internal.Stringify (unsafeStringify)
import Generated.Schema.Gql as Schema
import GraphQL.Client.Args ((=>>))
import GraphQL.Client.BaseClients.Apollo (ApolloSubClient, createSubscriptionClient)
import GraphQL.Client.Types (class GqlQuery, Client)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.GraphQL.Error (GqlFailure)
import Halogen.GraphQL.GqlRemote (GqlRemote)
import Halogen.GraphQL.Hooks.Mutation (useMutation)
import Halogen.GraphQL.Hooks.Subscription (GqlSubscriptionFoldHook, useSubscriptionAppend, useSubscriptionAppendFullRes)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    client :: GqlClient <-
      liftEffect
        $ createSubscriptionClient
            { url: "http://localhost:4000/graphql"
            , authToken: Nothing
            , headers: []
            , websocketUrl: "ws://localhost:4000/subscriptions"
            }
    runUI app { client } body
  where
  app :: forall q o. H.Component q { client :: GqlClient } o Aff
  app =
    Hooks.component \_ { client } -> Hooks.do
      Hooks.pure do
        HH.div
          [ HP.style "margin:1rem" ]
          [ HH.slot_ (Proxy :: Proxy "newPost") unit newPost { client }
          , HH.slot_ (Proxy :: Proxy "viewPosts") unit viewPosts { client }
          ]

  newPost :: forall q o. H.Component q { client :: GqlClient } o Aff
  newPost =
    Hooks.component \_ { client } -> Hooks.do
      comment /\ commentId <- Hooks.useState ""
      author /\ authorId <- Hooks.useState ""
      let
        mutateNewPost ev = do
          liftEffect $ preventDefault ev
          void $ client
            # mutation "new_post"
                { addPost: { author, comment } =>> { id: unit }
                }
      Hooks.pure do
        HH.form
          [ HE.onSubmit mutateNewPost ]
          [ HH.div
              [ HP.style "margin:1rem"]
              [ HH.div_ [HH.label [] [ HH.text "author" ]]
              , HH.input [ HE.onValueChange (Hooks.put authorId), HP.value author ]
              ]
          , HH.div
              [ HP.style "margin:1rem"]
              [ HH.div_ [HH.label [] [ HH.text "comment" ]]
              , HH.input [ HE.onValueChange (Hooks.put commentId), HP.value comment ]
              ]
          , HH.div
              [ HP.style "margin:1rem"]
              [HH.input [ HP.type_ InputSubmit,  HP.value "Add post" ]]
          ]

  viewPosts :: forall q o. H.Component q { client :: GqlClient } o Aff
  viewPosts =
    Hooks.component \_ { client } -> Hooks.do
      res <- client # subscription "new_post" { postAdded: { author: unit, comment: unit, id: unit } }
      fullRes <- client # subscriptionFullRes "new_post" { postAdded: { author: unit, comment: unit, id: unit } }

      Hooks.pure do
        HH.div
          [ HP.style "margin-top:2rem" ]
          [ HH.div_ [ HH.text "Query result:" ]
          , HH.div_ $ res <#> \p -> case p of 
            Left err -> HH.div_ [ HH.text "Error:", HH.pre_ [ HH.text $ show err ] ]
            Right post ->  HH.div_ [ HH.text "New post:", HH.pre_ [ HH.text $ stringify $ encodeJson post ] ]

          , HH.div_ [ HH.text "Query full result:" ]
          , HH.div_ $ fullRes <#> \post -> 
              HH.div_ [ HH.text "New post:", HH.pre_ [ HH.text $ unsafeStringify post ] ]

          ]


  subscription ::
    forall m query res.
    MonadAff m =>
    DecodeJson res =>
    GqlQuery Schema.Subscription query res =>
    String -> query -> GqlClient -> GqlSubscriptionFoldHook m (Array (Either GqlFailure res))
  subscription = useSubscriptionAppend decodeJson identity

  subscriptionFullRes = useSubscriptionAppendFullRes decodeJson identity

  mutation ::
    forall m query res.
    MonadAff m =>
    DecodeJson res =>
    GqlQuery Schema.Mutation query res =>
    String -> query -> GqlClient -> HookM m (GqlRemote res)
  mutation = useMutation decodeJson identity

-- Client type
type GqlClient
  = Client ApolloSubClient Schema.Query Schema.Mutation Schema.Subscription
