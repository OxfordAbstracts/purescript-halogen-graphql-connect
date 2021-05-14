module Generated.Schema.Gql where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import GraphQL.Client.Args (class ArgGql, class RecordArg, type (==>), NotNull)
import GraphQL.Client.ID (ID)
import Generated.Enum.Colour (Colour)


type Query = 
  { prop :: (Maybe String)
  , widgets :: 
    { id :: Int
    }
    ==> (Array Widget)
  }

type Widget = 
  { id :: Int
  , name :: String
  , colour :: Colour
  }

type Mutation = 
  { set_widget_colour :: 
    { id :: (NotNull Int)
    , colour :: (NotNull Colour)
    }
    ==> Int
  }
