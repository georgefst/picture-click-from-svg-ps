{ name = "picture-click-from-svg"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "halogen"
  , "node-fs"
  , "parsing"
  , "polymorphic-vectors"
  , "psci-support"
  , "quickcheck"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
