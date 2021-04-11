{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-graphql-connect"
, dependencies =
  [ "argonaut"
  , "console"
  , "effect"
  , "graphql-client"
  , "halogen"
  , "psci-support"
  , "remotedata"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
