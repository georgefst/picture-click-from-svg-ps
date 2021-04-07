{ name = "picture-click-from-svg"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "halogen"
  , "node-fs-aff"
  , "parsing"
  , "polymorphic-vectors"
  , "psci-support"
  , "quickcheck"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
