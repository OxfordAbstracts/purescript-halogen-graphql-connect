{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-generic"
  , "graphql-client"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "record"
  , "remotedata"
  , "tuples"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "../../src/**/*.purs", "test/**/*.purs" ]
}
