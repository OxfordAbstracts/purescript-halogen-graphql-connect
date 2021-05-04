module Halogen.GraphQL.GqlRemote where 

import GraphQL.Client.Types (GqlRes)
import Halogen.GraphQL.Error (GqlFailure)
import Network.RemoteData (RemoteData)

type GqlRemote res
  = RemoteData GqlFailure res

type GqlRemoteFull res = GqlRemote (GqlRes res)