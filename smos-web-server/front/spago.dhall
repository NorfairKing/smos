{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "codec-argonaut"
  , "console"
  , "css"
  , "debug"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "nonempty"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
