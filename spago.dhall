{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-graphql-connect"
, repository = "https://github.com/OxfordAbstracts/purescript-halogen-graphql-connect"
, license = "MIT"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "graphql-client"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "record"
  , "remotedata"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
