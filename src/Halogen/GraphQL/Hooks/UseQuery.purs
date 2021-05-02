module Halogen.GraphQL.Hooks.UseQuery where
-- module Halogen.GraphQL.Hooks.UseQuery (UseQuery, useQuery) where

-- import Prelude

-- import Data.Foldable (traverse_)
-- import Data.Maybe (Maybe(..))
-- import Data.Tuple.Nested ((/\))
-- import Effect.Aff (Fiber, Milliseconds, delay, error, forkAff, killFiber)
-- import Effect.Aff.AVar (AVar)
-- import Effect.Aff.AVar as AVar
-- import Effect.Aff.Class (class MonadAff, liftAff)
-- import Effect.Class (liftEffect)
-- import Effect.Ref as Ref
-- import Halogen.Hooks (Hook, HookM, UseRef)
-- import Halogen.Hooks as Hooks


-- newtype UseQuery schema query hooks =
--   UseQuery (UseRef (Maybe query) (UseRef (Maybe GraphQLQuery) hooks))


-- type GraphQLQuery = {}


-- useQuery :: forall m schema query res hooks a
--   . MonadAff m => 
--   query -> Hook m (UseQuery schema query hooks) (query -> HookM m Unit) res
-- useQuery query = pure ?d
