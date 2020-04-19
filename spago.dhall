{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-monad-control"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "freet"
  , "functors"
  , "identity"
  , "lists"
  , "prelude"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/athanclark/purescript-monad-control.git"
}
