module Generated.Schema.Gql where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import GraphQL.Client.Args (class ArgGql, class RecordArg, type (==>), NotNull)
import GraphQL.Client.ID (ID)



type Subscription = 
  { postAdded :: Post
  }

type Query = 
  { posts :: (Array Post)
  }

type Mutation = 
  { addPost :: 
    { author :: String
    , comment :: String
    }
    ==> (Maybe Post)
  }

type Post = 
  { id :: Int
  , author :: String
  , comment :: String
  }
