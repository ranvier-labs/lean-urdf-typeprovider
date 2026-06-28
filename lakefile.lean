import Lake

open Lake DSL

package «LeanUrdfTypeProvider»

require LeanTest from "/Users/pehle/dev/lean_test"

@[default_target]
lean_lib LeanUrdfTypeProvider where
  roots := #[`LeanUrdfTypeProvider]
  precompileModules := true

lean_lib LeanUrdfTypeProviderTest where
  roots := #[`LeanUrdfTypeProviderTest]
  precompileModules := true

@[test_driver]
lean_exe test where
  root := `TestDriver
  supportInterpreter := true
