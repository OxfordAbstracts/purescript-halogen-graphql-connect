{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-graphql-connect"
, dependencies =
  [ "aff"
  , "argonaut"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "graphql-client"
  , "halogen"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "record"
  , "remotedata"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
