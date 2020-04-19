{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "example"
, dependencies =
  [ "console", "effect", "monad-control", "prelude", "psci-support", "queue" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
