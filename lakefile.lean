import Lake

open Lake DSL

package «LeanUrdfTypeProvider»

require LeanTest from git "https://github.com/cpehle/lean_test.git" @
  "b42cd3d78716e5a2de5b640ac82d7fe3f05f2a4c"

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
