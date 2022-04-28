{ name = "wedstrijdvolgorde"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "integers"
  , "lists"
  , "maybe"
  , "nonempty"
  , "partial"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "transformers"
  , "tuples"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
