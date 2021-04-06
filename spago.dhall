{ name = "picture-click-from-svg"
, dependencies =
  [ "console", "effect", "halogen", "node-fs", "psci-support", "quickcheck" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
