module Halogen.GraphQL.Internal.Util where

import Prelude
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen.GraphQL.Error (GqlFailure(..))

checkErrorsAndDecode ::
  forall res.
  Boolean ->
  (Json -> Either JsonDecodeError res) ->
  Json -> Either GqlFailure res
checkErrorsAndDecode checkErrors decoder json =
  if checkErrors then case getErrors json of
    Just errors -> Left $ QueryError errors
    _ -> lmap DecodeError $ decoder json
  else
    lmap DecodeError $ decoder json
  where
  getErrors json_ = case decodeJson json_ of
    Right ({ errors } :: { errors :: _ }) -> Just errors
    _ -> Nothing
