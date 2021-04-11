{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "console"
  , "debug"
  , "effect"
  , "graphql-client"
  , "halogen"
  , "psci-support"
  , "remotedata"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "../../src/**/*.purs", "test/**/*.purs" ]
}
