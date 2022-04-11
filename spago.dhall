{ name = "wedstrijdvolgorde"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "foreign"
  , "halogen"
  , "lists"
  , "maybe"
  , "nonempty"
  , "partial"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "transformers"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
