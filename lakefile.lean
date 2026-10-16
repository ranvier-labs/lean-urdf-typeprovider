import Lake

open Lake DSL

package «LeanUrdfTypeProvider»

require LeanTest from git "https://github.com/ranvier-labs/lean_test.git" @
  "2778a0b4bec3eaa6a6d00f077750da4ba5b2e314"

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
